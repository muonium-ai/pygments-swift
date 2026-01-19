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
        XCTAssertNotNil(LexerRegistry.makeLexer(languageName: "ts"))
        XCTAssertNotNil(LexerRegistry.makeLexer(languageName: "c"))
        XCTAssertNotNil(LexerRegistry.makeLexer(languageName: "cpp"))
        XCTAssertNotNil(LexerRegistry.makeLexer(languageName: "csharp"))
        XCTAssertNotNil(LexerRegistry.makeLexer(languageName: "go"))
        XCTAssertNotNil(LexerRegistry.makeLexer(languageName: "rust"))
        XCTAssertNil(LexerRegistry.makeLexer(languageName: "unknown"))
    }

    func testMakeLexerByFilename() {
        XCTAssertNotNil(LexerRegistry.makeLexer(filename: "main.swift"))
        XCTAssertNotNil(LexerRegistry.makeLexer(filename: "data.json"))
        XCTAssertNotNil(LexerRegistry.makeLexer(filename: "data.jsonld"))
        XCTAssertNotNil(LexerRegistry.makeLexer(filename: "app.py"))
        XCTAssertNotNil(LexerRegistry.makeLexer(filename: "app.js"))
        XCTAssertNotNil(LexerRegistry.makeLexer(filename: "App.java"))
        XCTAssertNotNil(LexerRegistry.makeLexer(filename: "App.ts"))
        XCTAssertNotNil(LexerRegistry.makeLexer(filename: "App.tsx"))
        XCTAssertNotNil(LexerRegistry.makeLexer(filename: "main.c"))
        XCTAssertNotNil(LexerRegistry.makeLexer(filename: "main.h"))
        XCTAssertNotNil(LexerRegistry.makeLexer(filename: "main.cpp"))
        XCTAssertNotNil(LexerRegistry.makeLexer(filename: "main.hpp"))
        XCTAssertNotNil(LexerRegistry.makeLexer(filename: "Program.cs"))
        XCTAssertNotNil(LexerRegistry.makeLexer(filename: "main.go"))
        XCTAssertNotNil(LexerRegistry.makeLexer(filename: "main.rs"))
        XCTAssertNil(LexerRegistry.makeLexer(filename: "README.md"))
    }
}
