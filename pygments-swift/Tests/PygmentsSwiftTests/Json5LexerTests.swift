import XCTest
@testable import PygmentsSwift

final class Json5LexerTests: XCTestCase {
    func testJson5Basics() {
        let lexer = Json5Lexer()
        let input = """
        // comment
        {
          unquoted: 'str',
          ok: true,
          n: 0x10,
        }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .keyword) && $0.value == "true" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
