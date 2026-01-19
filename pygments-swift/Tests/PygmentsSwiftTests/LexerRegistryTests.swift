import XCTest
@testable import PygmentsSwift

final class LexerRegistryTests: XCTestCase {
    func testMakeLexerByName() {
        XCTAssertNotNil(LexerRegistry.makeLexer(languageName: "swift"))
        XCTAssertNotNil(LexerRegistry.makeLexer(languageName: "json"))
        XCTAssertNotNil(LexerRegistry.makeLexer(languageName: "jsonld"))
        XCTAssertNotNil(LexerRegistry.makeLexer(languageName: "python"))
        XCTAssertNotNil(LexerRegistry.makeLexer(languageName: "js"))
        XCTAssertNotNil(LexerRegistry.makeLexer(languageName: "java"))
        XCTAssertNil(LexerRegistry.makeLexer(languageName: "unknown"))
    }

    func testMakeLexerByFilename() {
        XCTAssertNotNil(LexerRegistry.makeLexer(filename: "main.swift"))
        XCTAssertNotNil(LexerRegistry.makeLexer(filename: "data.json"))
        XCTAssertNotNil(LexerRegistry.makeLexer(filename: "data.jsonld"))
        XCTAssertNotNil(LexerRegistry.makeLexer(filename: "app.py"))
        XCTAssertNotNil(LexerRegistry.makeLexer(filename: "app.js"))
        XCTAssertNotNil(LexerRegistry.makeLexer(filename: "App.java"))
        XCTAssertNil(LexerRegistry.makeLexer(filename: "README.md"))
    }
}
