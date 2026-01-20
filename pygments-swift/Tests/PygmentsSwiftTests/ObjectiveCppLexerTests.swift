import XCTest
@testable import PygmentsSwift

final class ObjectiveCppLexerTests: XCTestCase {
    func testObjectiveCppBasics() {
        let lexer = ObjectiveCppLexer()
        let input = """
        // comment
        namespace demo { int x = 0; }
        int main(void) { @autoreleasepool { NSLog(@"hi"); } return 0; }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(300).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "namespace" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) && $0.value.contains("@\"hi\"") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
