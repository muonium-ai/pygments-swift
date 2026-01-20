import XCTest
@testable import PygmentsSwift

final class ApacheConfLexerTests: XCTestCase {
    func testApacheConfBasics() {
        let lexer = ApacheConfLexer()
        let input = """
        # comment
        Listen 80
        <VirtualHost *:80>
          ErrorLog ${APACHE_LOG_DIR}/error.log
        </VirtualHost>
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(450).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .name) && $0.value == "Listen" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value.contains("<VirtualHost") }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .name) && $0.value.contains("${APACHE_LOG_DIR}") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
