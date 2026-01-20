import XCTest
@testable import PygmentsSwift

final class XmlLexerTests: XCTestCase {
    func testXmlBasics() {
        let lexer = XmlLexer()
        let input = """
        <?xml version=\"1.0\"?>
        <root>
          <node key=\"value\" />
          <![CDATA[ raw <xml> ]]>
        </root>
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(300).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Tag") && $0.value == "root" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) && $0.value.contains("CDATA") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
