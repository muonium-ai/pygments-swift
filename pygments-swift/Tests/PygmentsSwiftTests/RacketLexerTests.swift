import XCTest
@testable import PygmentsSwift

final class RacketLexerTests: XCTestCase {
    func testRacketBasics() {
        let lexer = RacketLexer()
        let input = """
        #lang racket
        ;; comment
        (define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(450).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) && $0.value.contains("#lang") }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "define" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
