import XCTest
@testable import PygmentsSwift

final class CommonLispLexerTests: XCTestCase {
    func testCommonLispBasics() {
        let lexer = CommonLispLexer()
        let input = """
        ;; comment
        (defun fib (n)
          (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
        (format t "fib(10)=~A" (fib 10))
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(500).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "defun" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) && $0.value.contains("fib(10)") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
