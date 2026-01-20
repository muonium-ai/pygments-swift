import XCTest
@testable import PygmentsSwift

final class TealLexerTests: XCTestCase {
    func testTealBasics() {
        let lexer = TealLexer()
        let input = """
        -- comment
        local function fib(n: integer): integer
          if n < 2 then
            return n
          end
          return fib(n - 1) + fib(n - 2)
        end
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "local" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "function" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
