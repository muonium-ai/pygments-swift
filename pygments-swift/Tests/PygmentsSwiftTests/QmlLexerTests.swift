import XCTest
@testable import PygmentsSwift

final class QmlLexerTests: XCTestCase {
    func testQmlBasics() {
        let lexer = QmlLexer()
        let input = """
        // comment
        import QtQuick 2.0
        Item { property int n: 10; function fib(x) { return x; } }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "import" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "function" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
