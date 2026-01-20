import XCTest
@testable import PygmentsSwift

final class PascalLexerTests: XCTestCase {
    func testPascalBasics() {
        let lexer = PascalLexer()
        let input = """
        { comment }
        program Fibonacci;
        function fib(n: Integer): Integer;
        begin
          if n < 2 then fib := n else fib := fib(n-1) + fib(n-2);
        end;
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(500).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value.lowercased() == "program" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .operator) && $0.value == ":=" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
