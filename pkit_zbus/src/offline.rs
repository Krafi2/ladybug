//! # D-Bus interface proxy for: `org.freedesktop.PackageKit.Offline`
//!
//! This code was generated by `zbus-xmlgen` `5.1.0` from D-Bus introspection data.
//! Source: `Interface '/org/freedesktop/PackageKit' from service 'org.freedesktop.PackageKit' on system bus`.
//!
//! You may prefer to adapt it, instead of using it verbatim.
//!
//! More information can be found in the [Writing a client proxy] section of the zbus
//! documentation.
//!
//! This type implements the [D-Bus standard interfaces], (`org.freedesktop.DBus.*`) for which the
//! following zbus API can be used:
//!
//! * [`zbus::fdo::PropertiesProxy`]
//! * [`zbus::fdo::IntrospectableProxy`]
//! * [`zbus::fdo::PeerProxy`]
//!
//! Consequently `zbus-xmlgen` did not generate code for the above interfaces.
//!
//! [Writing a client proxy]: https://dbus2.github.io/zbus/client.html
//! [D-Bus standard interfaces]: https://dbus.freedesktop.org/doc/dbus-specification.html#standard-interfaces,
use zbus::proxy;
#[proxy(
    interface = "org.freedesktop.PackageKit.Offline",
    default_service = "org.freedesktop.PackageKit",
    default_path = "/org/freedesktop/PackageKit"
)]
pub trait Offline {
    /// Cancel method
    fn cancel(&self) -> zbus::Result<()>;

    /// ClearResults method
    fn clear_results(&self) -> zbus::Result<()>;

    /// GetPrepared method
    fn get_prepared(&self) -> zbus::Result<Vec<String>>;

    /// Trigger method
    fn trigger(&self, action: &str) -> zbus::Result<()>;

    /// TriggerUpgrade method
    fn trigger_upgrade(&self, action: &str) -> zbus::Result<()>;

    /// PreparedUpgrade property
    #[zbus(property)]
    fn prepared_upgrade(
        &self,
    ) -> zbus::Result<std::collections::HashMap<String, zbus::zvariant::OwnedValue>>;

    /// TriggerAction property
    #[zbus(property)]
    fn trigger_action(&self) -> zbus::Result<String>;

    /// UpdatePrepared property
    #[zbus(property)]
    fn update_prepared(&self) -> zbus::Result<bool>;

    /// UpdateTriggered property
    #[zbus(property)]
    fn update_triggered(&self) -> zbus::Result<bool>;

    /// UpgradePrepared property
    #[zbus(property)]
    fn upgrade_prepared(&self) -> zbus::Result<bool>;

    /// UpgradeTriggered property
    #[zbus(property)]
    fn upgrade_triggered(&self) -> zbus::Result<bool>;
}
