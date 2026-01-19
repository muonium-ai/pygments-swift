import Foundation

public enum RegexOpt {
    /// Generates a (reasonably) optimized regex that matches any literal string in `strings`.
    ///
    /// This mirrors Pygments' `regexopt.regex_opt()` logic, but returns a regex *pattern string*.
    public static func regexOpt(_ strings: [String], prefix: String = "", suffix: String = "") -> String {
        let sorted = strings.sorted()
        return prefix + regexOptInner(sorted, openParen: "(") + suffix
    }

    private static func regexOptInner(_ strings: [String], openParen: String) -> String {
        let closeParen = openParen.isEmpty ? "" : ")"
        guard let first = strings.first else { return "" }

        if strings.count == 1 {
            return openParen + escape(first) + closeParen
        }

        if first.isEmpty {
            return openParen + regexOptInner(Array(strings.dropFirst()), openParen: "(?:") + "?" + closeParen
        }

        if first.count == 1 {
            var oneLetter: [String] = []
            var rest: [String] = []
            for s in strings {
                if s.count == 1 { oneLetter.append(s) } else { rest.append(s) }
            }
            if oneLetter.count > 1 {
                if !rest.isEmpty {
                    return openParen + regexOptInner(rest, openParen: "") + "|" + makeCharset(oneLetter) + closeParen
                }
                return openParen + makeCharset(oneLetter) + closeParen
            }
        }

        let prefix = commonPrefix(strings)
        if !prefix.isEmpty {
            let plen = prefix.count
            let remainder = strings.map { String($0.dropFirst(plen)) }
            return openParen + escape(prefix) + regexOptInner(remainder, openParen: "(?:") + closeParen
        }

        // Suffix
        let reversed = strings.map { String($0.reversed()) }
        let suffixRev = commonPrefix(reversed)
        if !suffixRev.isEmpty {
            let slen = suffixRev.count
            let trimmed = strings.map { String($0.dropLast(slen)) }.sorted()
            let suffix = String(suffixRev.reversed())
            return openParen + regexOptInner(trimmed, openParen: "(?:") + escape(suffix) + closeParen
        }

        // Last resort: split into groups based on whether first char equals first string's first char.
        let firstFirstChar = first.first!
        var groupA: [String] = []
        var groupB: [String] = []
        for s in strings {
            if s.first == firstFirstChar { groupA.append(s) } else { groupB.append(s) }
        }

        let parts = [groupA, groupB].filter { !$0.isEmpty }.map { regexOptInner($0, openParen: "") }
        return openParen + parts.joined(separator: "|") + closeParen
    }

    private static func escape(_ s: String) -> String {
        NSRegularExpression.escapedPattern(for: s)
    }

    private static func makeCharset(_ letters: [String]) -> String {
        let joined = letters.joined()
        // Escape characters special inside []: [ ^ \ - ]
        let escaped = joined
            .replacingOccurrences(of: "\\", with: "\\\\")
            .replacingOccurrences(of: "-", with: "\\-")
            .replacingOccurrences(of: "]", with: "\\]")
            .replacingOccurrences(of: "[", with: "\\[")
            .replacingOccurrences(of: "^", with: "\\^")
        return "[" + escaped + "]"
    }

    private static func commonPrefix(_ strings: [String]) -> String {
        guard var prefix = strings.first else { return "" }
        for s in strings.dropFirst() {
            prefix = commonPrefix(prefix, s)
            if prefix.isEmpty { break }
        }
        return prefix
    }

    private static func commonPrefix(_ a: String, _ b: String) -> String {
        let aChars = Array(a)
        let bChars = Array(b)
        let n = min(aChars.count, bChars.count)
        var out: [Character] = []
        out.reserveCapacity(n)
        for i in 0..<n {
            if aChars[i] != bChars[i] { break }
            out.append(aChars[i])
        }
        return String(out)
    }
}
