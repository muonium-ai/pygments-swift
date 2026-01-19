import Foundation

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
}

public enum StateTransition: Sendable, Hashable {
    case ops([StateOp])
}

public enum RuleAction: Sendable {
    case token(TokenType)
    case byGroups([TokenType?])
}

public enum TokenRuleDef: Sendable {
    case include(String)
    case rule(Rule)
}

public struct Rule: Sendable {
    public let regex: NSRegularExpression
    public let action: RuleAction?
    public let newState: StateTransition?

    public init(_ pattern: String, options: NSRegularExpression.Options = [], action: RuleAction?, newState: StateTransition? = nil) {
        self.regex = try! NSRegularExpression(pattern: pattern, options: options)
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

    public override init(options: LexerOptions = .init()) {
        super.init(options: options)
    }

    public final override func getTokens(_ text: String) -> [Token] {
        return getTokensUnprocessed(text)
    }

    public func getTokensUnprocessed(_ text: String, stack: [String] = ["root"]) -> [Token] {
        let processed = preprocess(text)
        let nsText = processed as NSString
        let end = nsText.length

        var ctx = LexerContext(pos: 0, stack: stack)
        var out: [Token] = []

        let compiledTokens = compileTokenDefsIfNeeded()

        func currentRules() -> [Rule] {
            let state = ctx.stack.last ?? "root"
            if !compiledTokens.isEmpty {
                return compiledTokens[state] ?? []
            }
            return tokens[state] ?? []
        }

        while true {
            if ctx.pos >= end { break }

            var matched = false
            for rule in currentRules() {
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
                        out.append(Token(start: ctx.pos, type: ttype, value: value))

                    case .byGroups(let groups):
                        // Groups correspond to capture groups 1..N.
                        for (idx, maybeType) in groups.enumerated() {
                            guard let ttype = maybeType else { continue }
                            let groupIndex = idx + 1
                            if groupIndex >= m.numberOfRanges { continue }
                            let r = m.range(at: groupIndex)
                            if r.location == NSNotFound || r.length == 0 { continue }
                            let value = nsText.substring(with: r)
                            out.append(Token(start: r.location, type: ttype, value: value))
                        }
                    }
                }

                ctx.pos = m.range.location + m.range.length

                if let transition = rule.newState {
                    apply(transition, to: &ctx)
                }

                break
            }

            if matched { continue }

            // No rule matched at this position.
            let ch = nsText.character(at: ctx.pos)
            if ch == 10 { // '\n'
                ctx.stack = ["root"]
                out.append(Token(start: ctx.pos, type: .whitespace, value: "\n"))
                ctx.pos += 1
                continue
            }

            // Emit error token for the single UTF-16 code unit.
            let r = NSRange(location: ctx.pos, length: 1)
            let value = nsText.substring(with: r)
            out.append(Token(start: ctx.pos, type: .error, value: value))
            ctx.pos += 1
        }

        return out
    }

    private func apply(_ transition: StateTransition, to ctx: inout LexerContext) {
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
                }
            }
        }
    }

    private func compileTokenDefsIfNeeded() -> [String: [Rule]] {
        let defs = tokenDefs
        guard !defs.isEmpty else { return [:] }

        var compiled: [String: [Rule]] = [:]
        var visiting: Set<String> = []

        func compileState(_ state: String) -> [Rule] {
            if let cached = compiled[state] { return cached }
            if visiting.contains(state) {
                // Defensive: avoid infinite recursion on cycles.
                return []
            }
            visiting.insert(state)

            var rules: [Rule] = []
            for def in defs[state] ?? [] {
                switch def {
                case .include(let other):
                    rules.append(contentsOf: compileState(other))
                case .rule(let rule):
                    rules.append(rule)
                }
            }

            visiting.remove(state)
            compiled[state] = rules
            return rules
        }

        // Compile all known states.
        for state in defs.keys {
            _ = compileState(state)
        }
        return compiled
    }
}
