// //! Serde utilities
//
// use serde::{
//     de::{self, MapAccess, Visitor},
//     Deserialize, Deserializer,
// };
//
// /// Deserialize a value of set a default using a fallible function.
// pub fn fallible_default<'de, F, T, E, D>(deserializer: D) -> Result<T, D::Error>
// where
//     F: FnOnce() -> Result<T, E>,
//     T: Deserialize<'de>,
//     D: Deserializer<'de>,
// {
//     // This is a Visitor that forwards string types to T's `FromStr` impl and
//     // forwards map types to T's `Deserialize` impl. The `PhantomData` is to
//     // keep the compiler from complaining about T being an unused generic type
//     // parameter. We need T in order to know the Value type for the Visitor
//     // impl.
//     struct StringOrStruct<T>(PhantomData<fn() -> T>);
//
//     impl<'de, T> Visitor<'de> for StringOrStruct<T>
//     where
//         T: Deserialize<'de> + FromStr,
//         <T as FromStr>::Err: std::fmt::Display,
//     {
//         type Value = T;
//
//         fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
//             formatter.write_str("string or map")
//         }
//
//         fn visit_str<E>(self, value: &str) -> Result<T, E>
//         where
//             E: de::Error,
//         {
//             FromStr::from_str(value).map_err(E::custom)
//         }
//
//         fn visit_map<M>(self, map: M) -> Result<T, M::Error>
//         where
//             M: MapAccess<'de>,
//         {
//             // `MapAccessDeserializer` is a wrapper that turns a `MapAccess`
//             // into a `Deserializer`, allowing it to be used as the input to T's
//             // `Deserialize` implementation. T then deserializes itself using
//             // the entries from the map visitor.
//             Deserialize::deserialize(de::value::MapAccessDeserializer::new(map))
//         }
//     }
//
//     deserializer.deserialize_any(StringOrStruct(PhantomData))
// }
