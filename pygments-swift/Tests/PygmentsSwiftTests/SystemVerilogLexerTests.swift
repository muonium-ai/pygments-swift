import XCTest
@testable import PygmentsSwift

final class SystemVerilogLexerTests: XCTestCase {
    func testSystemVerilogBasics() {
        let lexer = SystemVerilogLexer()
        let input = """
        // comment
        module fib;
          function automatic int f(input int n);
            if (n < 2) return n;
            return f(n-1) + f(n-2);
          endfunction
        endmodule
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(450).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "module" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "endmodule" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
