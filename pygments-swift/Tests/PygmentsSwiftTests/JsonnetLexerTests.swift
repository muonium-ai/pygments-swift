import XCTest
@testable import PygmentsSwift

final class JsonnetLexerTests: XCTestCase {
    func testJsonnetBasics() {
        let lexer = JsonnetLexer()
        let input = """
        // comment
        local fib(n) = if n < 2 then n else fib(n-1) + fib(n-2);
        { fib10: fib(10) }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "local" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "if" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
