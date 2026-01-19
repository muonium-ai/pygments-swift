import Foundation

public enum RegexHelpers {
    /// Builds a regex pattern matching any of the given literal `words`.
    ///
    /// Example: `words(["if","else"], suffix: "\\b")`.
    public static func words(_ words: [String], prefix: String = "", suffix: String = "") -> String {
        RegexOpt.regexOpt(words, prefix: prefix, suffix: suffix)
    }
}
