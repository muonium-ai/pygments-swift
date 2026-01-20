import XCTest
@testable import PygmentsSwift

final class DevicetreeLexerTests: XCTestCase {
    func testDevicetreeBasics() {
        let lexer = DevicetreeLexer()
        let input = #"""
        // comment
        /dts-v1/;
        / {
          compatible = "acme,board";
          led0: led@0 {
            label = "status";
          };
        };
        """#

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .name) && $0.value == "led0" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
