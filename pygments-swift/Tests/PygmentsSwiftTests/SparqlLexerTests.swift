import XCTest
@testable import PygmentsSwift

final class SparqlLexerTests: XCTestCase {
    func testSparqlBasics() {
        let lexer = SparqlLexer()
        let input = """
        # comment
        PREFIX ex: <http://example.com/>
        SELECT ?s WHERE { ?s ex:n 10 . }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "SELECT" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .name) && $0.value.contains("?") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
