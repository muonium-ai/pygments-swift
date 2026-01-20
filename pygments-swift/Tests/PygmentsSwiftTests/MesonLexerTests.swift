import XCTest
@testable import PygmentsSwift

final class MesonLexerTests: XCTestCase {
    func testMesonBasics() {
        let lexer = MesonLexer()
        let input = """
        # comment
        project('fibonacci', 'c')
        executable('fib', 'fib.c')
        if true
          message('ok')
        endif
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "project" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) && $0.value.contains("fibonacci") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
