import XCTest
@testable import PygmentsSwift

final class ActionScriptLexerTests: XCTestCase {
    func testActionScriptBasics() {
        let lexer = ActionScriptLexer()
        let input = #"""
        package {
          public class Foo {
            public function bar():void {
              var x = 1;
              return;
            }
          }
        }
        // comment
        """#

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(300).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .keyword) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .number) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
