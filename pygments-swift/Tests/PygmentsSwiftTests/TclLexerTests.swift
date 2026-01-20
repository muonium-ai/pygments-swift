import XCTest
@testable import PygmentsSwift

final class TclLexerTests: XCTestCase {
    func testTclBasics() {
        let lexer = TclLexer()
        let input = """
        # comment
        proc fib {n} { if {$n < 2} { return $n } }
        puts "fib(10)=[fib 10]"
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "proc" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .name) && $0.value.contains("$n") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
