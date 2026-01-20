import XCTest
@testable import PygmentsSwift

final class YaraLexerTests: XCTestCase {
    func testYaraBasics() {
        let lexer = YaraLexer()
        let input = """
        // comment
        rule Fibonacci {
          strings:
            $a = "fib" nocase
          condition:
            $a
        }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "rule" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) && $0.value.contains("fib") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
