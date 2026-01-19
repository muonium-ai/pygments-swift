import Foundation
import ObjectiveC.runtime

public struct LexerContext: Sendable {
    public var pos: Int
    public var stack: [String]

    public init(pos: Int = 0, stack: [String] = ["root"]) {
        self.pos = pos
        self.stack = stack
    }
}

public enum StateOp: Sendable, Hashable {
    case pop
    case popN(Int)
    case pushCurrent
    case push(String)
    /// Push a temporary state composed from the rules of the listed states.
    case pushCombined([String])
}

public enum StateTransition: Sendable, Hashable {
    case ops([StateOp])
}

public enum RuleAction: Sendable {
    case token(TokenType)
    case byGroups([TokenType?])

    /// Process the matched substring using another lexer, emitting its tokens with an offset.
    ///
    /// Mirrors Pygments' `using(OtherLexer, state=...)` callback, but restricted to RegexLexer subclasses.
    case using(RegexLexer.Type, stack: [String]?)

    /// Process the matched substring using the current lexer's type.
    case usingThis(stack: [String]?)
}

public enum TokenRuleDef: Sendable {
    case include(String)
    case rule(Rule)

    /// A default transition rule (zero-length match) for a state.
    /// Mirrors Pygments' `default(...)`.
    case `default`(StateTransition)

    /// Insert rules from the superclass' state definition at this point.
    /// Mirrors Pygments' `inherit` marker.
    case inherit
}

public struct Rule: Sendable {
    public let regex: NSRegularExpression
    public let action: RuleAction?
    public let newState: StateTransition?

    public init(_ pattern: String, options: NSRegularExpression.Options = [], action: RuleAction?, newState: StateTransition? = nil) {
        // NSRegularExpression rejects an empty pattern, but we sometimes need
        // a zero-length match for Pygments-like `default(...)` transitions.
        let pat = pattern.isEmpty ? "(?:)" : pattern
        self.regex = try! NSRegularExpression(pattern: pat, options: options)
        self.action = action
        self.newState = newState
    }
}

open class RegexLexer: LexerBase {
    /// State -> ordered rules. Rules are matched at the current position.
    open var tokens: [String: [Rule]] { [:] }

    /// Optional higher-level token definitions which can `include` other states.
    /// If non-empty, this takes precedence over `tokens`.
    open var tokenDefs: [String: [TokenRuleDef]] { [:] }

    public required init(options: LexerOptions = .init()) {
        super.init(options: options)
    }

    public final override func getTokens(_ text: String) -> [Token] {
        return getTokensUnprocessed(text)
    }

    public func getTokensUnprocessed(_ text: String, stack: [String] = ["root"]) -> [Token] {
        return lex(text, preprocessInput: true, stack: stack)
    }

    /// Like Pygments' `get_tokens_unprocessed`: does not apply `preprocess()`.
    public func getTokensUnprocessedRaw(_ text: String, stack: [String] = ["root"]) -> [Token] {
        return lex(text, preprocessInput: false, stack: stack)
    }

    private func lex(_ text: String, preprocessInput: Bool, stack: [String]) -> [Token] {
        let processed = preprocessInput ? preprocess(text) : text
        let nsText = processed as NSString
        let end = nsText.length

        // Map UTF-16 offsets -> Unicode-scalar offsets (codepoint indices).
        // We precompute on scalar boundaries so we can translate match/group NSRange locations.
        var utf16ToScalar: [Int: Int] = [:]
        utf16ToScalar.reserveCapacity(processed.unicodeScalars.count + 1)
        var utf16Offset = 0
        var scalarOffset = 0
        utf16ToScalar[0] = 0
        for scalar in processed.unicodeScalars {
            utf16Offset += scalar.utf16.count
            scalarOffset += 1
            utf16ToScalar[utf16Offset] = scalarOffset
        }

        func scalarIndex(forUTF16Offset off: Int) -> Int {
            // Most regex boundaries will align with scalar boundaries.
            if let mapped = utf16ToScalar[off] { return mapped }
            // Fallback: find nearest lower boundary.
            // This is conservative if a regex split a surrogate pair.
            var candidate = off
            while candidate > 0 {
                candidate -= 1
                if let mapped = utf16ToScalar[candidate] {
                    return mapped
                }
            }
            return 0
        }

        var ctx = LexerContext(pos: 0, stack: stack)
        var out: [Token] = []

        var compiledTokens = compileTokenDefsIfNeeded()
        var dynamicTokens: [String: [Rule]] = [:]
        var tmpCounter = 0

        func delegationStack(_ override: [String]?) -> [String] {
            // Mirrors Pygments `using(..., state=...)` behavior:
            // - nil => start at root
            // - list/tuple => explicit stack
            // - single string => ('root', string)
            guard let override, !override.isEmpty else {
                return ["root"]
            }
            if override.count == 1, override.first != "root" {
                return ["root"] + override
            }
            return override
        }

        func currentRules() -> [Rule] {
            let state = ctx.stack.last ?? "root"
            if !compiledTokens.isEmpty {
                return dynamicTokens[state] ?? compiledTokens[state] ?? []
            }
            return dynamicTokens[state] ?? tokens[state] ?? []
        }

        while true {
            if ctx.pos >= end { break }

            var matched = false
            for rule in currentRules() {
                let priorPos = ctx.pos
                let priorState = ctx.stack.last ?? "root"

                let searchRange = NSRange(location: ctx.pos, length: end - ctx.pos)
                guard let m = rule.regex.firstMatch(in: processed, options: [], range: searchRange) else {
                    continue
                }
                guard m.range.location == ctx.pos else {
                    continue
                }

                matched = true
                if let action = rule.action {
                    switch action {
                    case .token(let ttype):
                        let value = nsText.substring(with: m.range)
                        out.append(Token(start: ctx.pos, startScalar: scalarIndex(forUTF16Offset: ctx.pos), type: ttype, value: value))

                    case .byGroups(let groups):
                        // Groups correspond to capture groups 1..N.
                        for (idx, maybeType) in groups.enumerated() {
                            guard let ttype = maybeType else { continue }
                            let groupIndex = idx + 1
                            if groupIndex >= m.numberOfRanges { continue }
                            let r = m.range(at: groupIndex)
                            if r.location == NSNotFound || r.length == 0 { continue }
                            let value = nsText.substring(with: r)
                            out.append(Token(start: r.location, startScalar: scalarIndex(forUTF16Offset: r.location), type: ttype, value: value))
                        }

                    case .using(let otherType, let stackOverride):
                        let sub = nsText.substring(with: m.range)
                        let other = otherType.init(options: self.options)
                        let subTokens = other.getTokensUnprocessedRaw(sub, stack: delegationStack(stackOverride))
                        let matchStartScalar = scalarIndex(forUTF16Offset: m.range.location)
                        for t in subTokens {
                            out.append(Token(start: t.start + m.range.location, startScalar: t.startScalar + matchStartScalar, type: t.type, value: t.value))
                        }

                    case .usingThis(let stackOverride):
                        let sub = nsText.substring(with: m.range)
                        // Pygments `using(this)` reuses the current lexer instance unless
                        // keyword args require instantiation. We don't have kwargs here, so
                        // always delegate to `self`.
                        let subTokens = self.getTokensUnprocessedRaw(sub, stack: delegationStack(stackOverride))
                        let matchStartScalar = scalarIndex(forUTF16Offset: m.range.location)
                        for t in subTokens {
                            out.append(Token(start: t.start + m.range.location, startScalar: t.startScalar + matchStartScalar, type: t.type, value: t.value))
                        }
                    }
                }

                ctx.pos = m.range.location + m.range.length

                if let transition = rule.newState {
                    apply(transition, to: &ctx, compiledTokens: &compiledTokens, dynamicTokens: &dynamicTokens, tmpCounter: &tmpCounter)
                }

                // Safety: if we matched a zero-length regex and neither position nor state changes,
                // we'd loop forever. Advance by one code unit as an Error token.
                let currentState = ctx.stack.last ?? "root"
                if m.range.length == 0 && ctx.pos == priorPos && currentState == priorState {
                    if priorPos < end {
                        let r = NSRange(location: priorPos, length: 1)
                        let value = nsText.substring(with: r)
                        out.append(Token(start: priorPos, startScalar: scalarIndex(forUTF16Offset: priorPos), type: .error, value: value))
                        ctx.pos = priorPos + 1
                    }
                }

                break
            }

            if matched { continue }

            // No rule matched at this position.
            // Mirror Pygments' RegexLexer behavior:
            // - if the unmatched character is a newline, reset state to root and emit Whitespace
            // - otherwise emit Error for the single UTF-16 code unit.
            let r = NSRange(location: ctx.pos, length: 1)
            let value = nsText.substring(with: r)
            if value == "\n" {
                ctx.stack = ["root"]
                out.append(Token(start: ctx.pos, startScalar: scalarIndex(forUTF16Offset: ctx.pos), type: .whitespace, value: value))
            } else {
                out.append(Token(start: ctx.pos, startScalar: scalarIndex(forUTF16Offset: ctx.pos), type: .error, value: value))
            }
            ctx.pos += 1
        }

        return out
    }

    private func apply(_ transition: StateTransition, to ctx: inout LexerContext) {
        // Backwards-compatible wrapper for callers that don't use combined.
        var compiledTokens: [String: [Rule]] = [:]
        var dynamicTokens: [String: [Rule]] = [:]
        var tmpCounter = 0
        apply(transition, to: &ctx, compiledTokens: &compiledTokens, dynamicTokens: &dynamicTokens, tmpCounter: &tmpCounter)
    }

    private func apply(
        _ transition: StateTransition,
        to ctx: inout LexerContext,
        compiledTokens: inout [String: [Rule]],
        dynamicTokens: inout [String: [Rule]],
        tmpCounter: inout Int
    ) {
        func lookupRules(_ state: String) -> [Rule] {
            if let dyn = dynamicTokens[state] { return dyn }
            if !compiledTokens.isEmpty {
                return compiledTokens[state] ?? []
            }
            return tokens[state] ?? []
        }

        switch transition {
        case .ops(let ops):
            for op in ops {
                switch op {
                case .pop:
                    if ctx.stack.count > 1 { ctx.stack.removeLast() }
                case .popN(let n):
                    guard n > 0 else { continue }
                    if n >= ctx.stack.count {
                        ctx.stack = ["root"]
                    } else {
                        for _ in 0..<n {
                            if ctx.stack.count > 1 { ctx.stack.removeLast() }
                        }
                    }
                case .pushCurrent:
                    if let top = ctx.stack.last {
                        ctx.stack.append(top)
                    } else {
                        ctx.stack = ["root", "root"]
                    }
                case .push(let state):
                    ctx.stack.append(state)
                case .pushCombined(let states):
                    // Build a temporary combined state.
                    let tmpState = "_tmp_\\(tmpCounter)"
                    tmpCounter += 1
                    var combined: [Rule] = []
                    for s in states {
                        combined.append(contentsOf: lookupRules(s))
                    }
                    dynamicTokens[tmpState] = combined
                    ctx.stack.append(tmpState)
                }
            }
        }
    }

    private func compileTokenDefsIfNeeded() -> [String: [Rule]] {
        let defs = tokenDefs
        guard !defs.isEmpty else { return [:] }

        // Compile parent first (if any).
        let parentCompiled = compileParentTokenDefsIfAny()

        var compiled: [String: [Rule]] = [:]
        var visiting: Set<String> = []

        func compileState(_ state: String) -> [Rule] {
            if let cached = compiled[state] { return cached }
            if visiting.contains(state) {
                // Defensive: avoid infinite recursion on cycles.
                return []
            }
            visiting.insert(state)

            let parentRules = parentCompiled[state] ?? []
            var rules: [Rule] = []

            if let localDefs = defs[state] {
                for def in localDefs {
                    switch def {
                    case .include(let other):
                        rules.append(contentsOf: compileState(other))
                    case .inherit:
                        rules.append(contentsOf: parentRules)
                    case .rule(let rule):
                        rules.append(rule)
                    case .default(let transition):
                        // Zero-length match; relies on state transition to make progress.
                        rules.append(Rule("", action: nil, newState: transition))
                    }
                }
            } else {
                // If the subclass doesn't define this state, inherit it fully.
                rules.append(contentsOf: parentRules)
            }

            visiting.remove(state)
            compiled[state] = rules
            return rules
        }

        // Compile all known states (local and inherited).
        for state in Set(defs.keys).union(parentCompiled.keys) {
            _ = compileState(state)
        }
        return compiled
    }

    private func compileParentTokenDefsIfAny() -> [String: [Rule]] {
        guard let superCls = class_getSuperclass(type(of: self)) as? RegexLexer.Type else {
            return [:]
        }
        let parent = superCls.init(options: self.options)
        // If parent doesn't use tokenDefs, we don't try to synthesize them.
        if parent.tokenDefs.isEmpty {
            return [:]
        }
        return parent.compileTokenDefsIfNeeded()
    }
}
