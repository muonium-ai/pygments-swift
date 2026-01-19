// swift-tools-version: 5.9

import PackageDescription

let package = Package(
    name: "PygmentsSwift",
    platforms: [
        .macOS(.v13)
    ],
    products: [
        .library(
            name: "PygmentsSwift",
            targets: ["PygmentsSwift"]
        )
    ],
    targets: [
        .target(
            name: "PygmentsSwift",
            dependencies: []
        ),
        .testTarget(
            name: "PygmentsSwiftTests",
            dependencies: ["PygmentsSwift"],
            resources: [
                .process("Fixtures")
            ]
        )
    ]
)
