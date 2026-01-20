import XCTest
@testable import PygmentsSwift

final class HamlLexerTests: XCTestCase {
    func testHamlBasics() {
        let lexer = HamlLexer()
        let input = """
        -# comment
        %div#fib.container
          - n = 10
          %span= "n=#{n}"
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(400).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Tag") && $0.value.contains("%div") }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) && $0.value.contains("n=#{n}") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
