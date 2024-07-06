//! This is an auto generated Rust Module

struct Person {
    name: String,
    age: i32,
    isStudent: bool,
    friends: Vec<String>,
    activity: Activity,
    coordinates: Vector,
}

enum Activity {
    Working,
    Studying { hours: i32, subject: Option<String> },
    Training { location: Location },
}

enum Location {
    Indoor,
    Outdoor,
}

struct Vector {
    x: i32,
    y: i32,
}
