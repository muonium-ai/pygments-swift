import XCTest
@testable import PygmentsSwift

final class JuliaLexerTests: XCTestCase {
    func testJuliaBasics() {
        let lexer = JuliaLexer()
        let input = """
        # comment
        function fib(n)
            if n < 2
                return n
            end
            return fib(n - 1) + fib(n - 2)
        end
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(300).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "function" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "end" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
