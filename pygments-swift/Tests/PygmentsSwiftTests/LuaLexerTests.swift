import XCTest
@testable import PygmentsSwift

final class LuaLexerTests: XCTestCase {
    func testLuaBasics() {
        let lexer = LuaLexer()
        let input = """
        -- comment
        local function fib(n)
          if n < 2 then return n end
          return fib(n - 1) + fib(n - 2)
        end
        print("ok")
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(250).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "function" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
