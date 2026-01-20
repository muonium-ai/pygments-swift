import XCTest
@testable import PygmentsSwift

final class VimLexerTests: XCTestCase {
    func testVimscriptBasics() {
        let lexer = VimLexer()
        let input = """
        " comment
        function! Fib(n)
          if a:n < 2
            return a:n
          endif
          return Fib(a:n-1) + Fib(a:n-2)
        endfunction

        let g:result = Fib(10)
        echo g:result
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value.lowercased() == "function" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Variable") && $0.value == "g:result" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
