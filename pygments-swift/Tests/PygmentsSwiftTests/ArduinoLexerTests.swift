import XCTest
@testable import PygmentsSwift

final class ArduinoLexerTests: XCTestCase {
    func testArduinoBasics() {
        let lexer = ArduinoLexer()
        let input = #"""
        #include <Arduino.h>
        void setup() {
          pinMode(13, OUTPUT);
        }
        void loop() {
          digitalWrite(13, HIGH);
        }
        // comment
        """#

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .name) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .number) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
