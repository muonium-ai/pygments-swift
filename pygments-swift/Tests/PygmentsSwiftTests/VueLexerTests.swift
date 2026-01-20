import XCTest
@testable import PygmentsSwift

final class VueLexerTests: XCTestCase {
    func testVueBasics() {
        let lexer = VueLexer()
        let input = #"""
        <template>
          <!-- comment -->
          <div class="greeting">Hello {{ name }}</div>
        </template>
        """#

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .name) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
