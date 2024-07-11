// This is an auto generated TypeScript Module

type Person = {
  name: string;
  age: number;
  isStudent: boolean;
  friends: Array<string>;
  activity?: null | Activity;
  coordinates: Vector;
};

type Activity =
  | { tag: "Working" }
  | { tag: "Studying"; value: { hours: number; subject?: null | string } }
  | { tag: "Training"; value: { place: Place } };

type Place = "Indoor" | "Outdoor";

type Vector = { x: number; y: number };
