import XCTest
@testable import PygmentsSwift

final class PugLexerTests: XCTestCase {
    func testPugBasics() {
        let lexer = PugLexer()
        let input = """
        //- comment
        mixin fib(n)
          span n=#{n}

        div#fib.container
          +fib(10)
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(450).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "mixin" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .number) && $0.value == "10" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
