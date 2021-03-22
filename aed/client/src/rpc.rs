// This file is generated. Do not edit
// @generated

// https://github.com/Manishearth/rust-clippy/issues/702
#![allow(unknown_lints)]
#![allow(clippy)]

#![cfg_attr(rustfmt, rustfmt_skip)]

#![allow(box_pointers)]
#![allow(dead_code)]
#![allow(missing_docs)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(trivial_casts)]
#![allow(unsafe_code)]
#![allow(unused_imports)]
#![allow(unused_results)]

use protobuf::Message as Message_imported_for_functions;
use protobuf::ProtobufEnum as ProtobufEnum_imported_for_functions;

#[derive(PartialEq,Clone,Default)]
pub struct ClientMsg {
    // message oneof groups
    msg: ::std::option::Option<ClientMsg_oneof_msg>,
    // special fields
    unknown_fields: ::protobuf::UnknownFields,
    cached_size: ::protobuf::CachedSize,
}

// see codegen.rs for the explanation why impl Sync explicitly
unsafe impl ::std::marker::Sync for ClientMsg {}

#[derive(Clone,PartialEq)]
pub enum ClientMsg_oneof_msg {
    setup(ClientMsg_Setup),
    player_connect(ClientMsg_PlayerConnect),
    player_disconnect(ClientMsg_PlayerDisconnect),
    player_input(ClientMsg_PlayerInput),
}

impl ClientMsg {
    pub fn new() -> ClientMsg {
        ::std::default::Default::default()
    }

    pub fn default_instance() -> &'static ClientMsg {
        static mut instance: ::protobuf::lazy::Lazy<ClientMsg> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ClientMsg,
        };
        unsafe {
            instance.get(ClientMsg::new)
        }
    }

    // .ClientMsg.Setup setup = 1;

    pub fn clear_setup(&mut self) {
        self.msg = ::std::option::Option::None;
    }

    pub fn has_setup(&self) -> bool {
        match self.msg {
            ::std::option::Option::Some(ClientMsg_oneof_msg::setup(..)) => true,
            _ => false,
        }
    }

    // Param is passed by value, moved
    pub fn set_setup(&mut self, v: ClientMsg_Setup) {
        self.msg = ::std::option::Option::Some(ClientMsg_oneof_msg::setup(v))
    }

    // Mutable pointer to the field.
    pub fn mut_setup(&mut self) -> &mut ClientMsg_Setup {
        if let ::std::option::Option::Some(ClientMsg_oneof_msg::setup(_)) = self.msg {
        } else {
            self.msg = ::std::option::Option::Some(ClientMsg_oneof_msg::setup(ClientMsg_Setup::new()));
        }
        match self.msg {
            ::std::option::Option::Some(ClientMsg_oneof_msg::setup(ref mut v)) => v,
            _ => panic!(),
        }
    }

    // Take field
    pub fn take_setup(&mut self) -> ClientMsg_Setup {
        if self.has_setup() {
            match self.msg.take() {
                ::std::option::Option::Some(ClientMsg_oneof_msg::setup(v)) => v,
                _ => panic!(),
            }
        } else {
            ClientMsg_Setup::new()
        }
    }

    pub fn get_setup(&self) -> &ClientMsg_Setup {
        match self.msg {
            ::std::option::Option::Some(ClientMsg_oneof_msg::setup(ref v)) => v,
            _ => ClientMsg_Setup::default_instance(),
        }
    }

    // .ClientMsg.PlayerConnect player_connect = 2;

    pub fn clear_player_connect(&mut self) {
        self.msg = ::std::option::Option::None;
    }

    pub fn has_player_connect(&self) -> bool {
        match self.msg {
            ::std::option::Option::Some(ClientMsg_oneof_msg::player_connect(..)) => true,
            _ => false,
        }
    }

    // Param is passed by value, moved
    pub fn set_player_connect(&mut self, v: ClientMsg_PlayerConnect) {
        self.msg = ::std::option::Option::Some(ClientMsg_oneof_msg::player_connect(v))
    }

    // Mutable pointer to the field.
    pub fn mut_player_connect(&mut self) -> &mut ClientMsg_PlayerConnect {
        if let ::std::option::Option::Some(ClientMsg_oneof_msg::player_connect(_)) = self.msg {
        } else {
            self.msg = ::std::option::Option::Some(ClientMsg_oneof_msg::player_connect(ClientMsg_PlayerConnect::new()));
        }
        match self.msg {
            ::std::option::Option::Some(ClientMsg_oneof_msg::player_connect(ref mut v)) => v,
            _ => panic!(),
        }
    }

    // Take field
    pub fn take_player_connect(&mut self) -> ClientMsg_PlayerConnect {
        if self.has_player_connect() {
            match self.msg.take() {
                ::std::option::Option::Some(ClientMsg_oneof_msg::player_connect(v)) => v,
                _ => panic!(),
            }
        } else {
            ClientMsg_PlayerConnect::new()
        }
    }

    pub fn get_player_connect(&self) -> &ClientMsg_PlayerConnect {
        match self.msg {
            ::std::option::Option::Some(ClientMsg_oneof_msg::player_connect(ref v)) => v,
            _ => ClientMsg_PlayerConnect::default_instance(),
        }
    }

    // .ClientMsg.PlayerDisconnect player_disconnect = 3;

    pub fn clear_player_disconnect(&mut self) {
        self.msg = ::std::option::Option::None;
    }

    pub fn has_player_disconnect(&self) -> bool {
        match self.msg {
            ::std::option::Option::Some(ClientMsg_oneof_msg::player_disconnect(..)) => true,
            _ => false,
        }
    }

    // Param is passed by value, moved
    pub fn set_player_disconnect(&mut self, v: ClientMsg_PlayerDisconnect) {
        self.msg = ::std::option::Option::Some(ClientMsg_oneof_msg::player_disconnect(v))
    }

    // Mutable pointer to the field.
    pub fn mut_player_disconnect(&mut self) -> &mut ClientMsg_PlayerDisconnect {
        if let ::std::option::Option::Some(ClientMsg_oneof_msg::player_disconnect(_)) = self.msg {
        } else {
            self.msg = ::std::option::Option::Some(ClientMsg_oneof_msg::player_disconnect(ClientMsg_PlayerDisconnect::new()));
        }
        match self.msg {
            ::std::option::Option::Some(ClientMsg_oneof_msg::player_disconnect(ref mut v)) => v,
            _ => panic!(),
        }
    }

    // Take field
    pub fn take_player_disconnect(&mut self) -> ClientMsg_PlayerDisconnect {
        if self.has_player_disconnect() {
            match self.msg.take() {
                ::std::option::Option::Some(ClientMsg_oneof_msg::player_disconnect(v)) => v,
                _ => panic!(),
            }
        } else {
            ClientMsg_PlayerDisconnect::new()
        }
    }

    pub fn get_player_disconnect(&self) -> &ClientMsg_PlayerDisconnect {
        match self.msg {
            ::std::option::Option::Some(ClientMsg_oneof_msg::player_disconnect(ref v)) => v,
            _ => ClientMsg_PlayerDisconnect::default_instance(),
        }
    }

    // .ClientMsg.PlayerInput player_input = 4;

    pub fn clear_player_input(&mut self) {
        self.msg = ::std::option::Option::None;
    }

    pub fn has_player_input(&self) -> bool {
        match self.msg {
            ::std::option::Option::Some(ClientMsg_oneof_msg::player_input(..)) => true,
            _ => false,
        }
    }

    // Param is passed by value, moved
    pub fn set_player_input(&mut self, v: ClientMsg_PlayerInput) {
        self.msg = ::std::option::Option::Some(ClientMsg_oneof_msg::player_input(v))
    }

    // Mutable pointer to the field.
    pub fn mut_player_input(&mut self) -> &mut ClientMsg_PlayerInput {
        if let ::std::option::Option::Some(ClientMsg_oneof_msg::player_input(_)) = self.msg {
        } else {
            self.msg = ::std::option::Option::Some(ClientMsg_oneof_msg::player_input(ClientMsg_PlayerInput::new()));
        }
        match self.msg {
            ::std::option::Option::Some(ClientMsg_oneof_msg::player_input(ref mut v)) => v,
            _ => panic!(),
        }
    }

    // Take field
    pub fn take_player_input(&mut self) -> ClientMsg_PlayerInput {
        if self.has_player_input() {
            match self.msg.take() {
                ::std::option::Option::Some(ClientMsg_oneof_msg::player_input(v)) => v,
                _ => panic!(),
            }
        } else {
            ClientMsg_PlayerInput::new()
        }
    }

    pub fn get_player_input(&self) -> &ClientMsg_PlayerInput {
        match self.msg {
            ::std::option::Option::Some(ClientMsg_oneof_msg::player_input(ref v)) => v,
            _ => ClientMsg_PlayerInput::default_instance(),
        }
    }
}

impl ::protobuf::Message for ClientMsg {
    fn is_initialized(&self) -> bool {
        if let Some(ClientMsg_oneof_msg::setup(ref v)) = self.msg {
            if !v.is_initialized() {
                return false;
            }
        }
        if let Some(ClientMsg_oneof_msg::player_connect(ref v)) = self.msg {
            if !v.is_initialized() {
                return false;
            }
        }
        if let Some(ClientMsg_oneof_msg::player_disconnect(ref v)) = self.msg {
            if !v.is_initialized() {
                return false;
            }
        }
        if let Some(ClientMsg_oneof_msg::player_input(ref v)) = self.msg {
            if !v.is_initialized() {
                return false;
            }
        }
        true
    }

    fn merge_from(&mut self, is: &mut ::protobuf::CodedInputStream) -> ::protobuf::ProtobufResult<()> {
        while !is.eof()? {
            let (field_number, wire_type) = is.read_tag_unpack()?;
            match field_number {
                1 => {
                    if wire_type != ::protobuf::wire_format::WireTypeLengthDelimited {
                        return ::std::result::Result::Err(::protobuf::rt::unexpected_wire_type(wire_type));
                    }
                    self.msg = ::std::option::Option::Some(ClientMsg_oneof_msg::setup(is.read_message()?));
                },
                2 => {
                    if wire_type != ::protobuf::wire_format::WireTypeLengthDelimited {
                        return ::std::result::Result::Err(::protobuf::rt::unexpected_wire_type(wire_type));
                    }
                    self.msg = ::std::option::Option::Some(ClientMsg_oneof_msg::player_connect(is.read_message()?));
                },
                3 => {
                    if wire_type != ::protobuf::wire_format::WireTypeLengthDelimited {
                        return ::std::result::Result::Err(::protobuf::rt::unexpected_wire_type(wire_type));
                    }
                    self.msg = ::std::option::Option::Some(ClientMsg_oneof_msg::player_disconnect(is.read_message()?));
                },
                4 => {
                    if wire_type != ::protobuf::wire_format::WireTypeLengthDelimited {
                        return ::std::result::Result::Err(::protobuf::rt::unexpected_wire_type(wire_type));
                    }
                    self.msg = ::std::option::Option::Some(ClientMsg_oneof_msg::player_input(is.read_message()?));
                },
                _ => {
                    ::protobuf::rt::read_unknown_or_skip_group(field_number, wire_type, is, self.mut_unknown_fields())?;
                },
            };
        }
        ::std::result::Result::Ok(())
    }

    // Compute sizes of nested messages
    #[allow(unused_variables)]
    fn compute_size(&self) -> u32 {
        let mut my_size = 0;
        if let ::std::option::Option::Some(ref v) = self.msg {
            match v {
                &ClientMsg_oneof_msg::setup(ref v) => {
                    let len = v.compute_size();
                    my_size += 1 + ::protobuf::rt::compute_raw_varint32_size(len) + len;
                },
                &ClientMsg_oneof_msg::player_connect(ref v) => {
                    let len = v.compute_size();
                    my_size += 1 + ::protobuf::rt::compute_raw_varint32_size(len) + len;
                },
                &ClientMsg_oneof_msg::player_disconnect(ref v) => {
                    let len = v.compute_size();
                    my_size += 1 + ::protobuf::rt::compute_raw_varint32_size(len) + len;
                },
                &ClientMsg_oneof_msg::player_input(ref v) => {
                    let len = v.compute_size();
                    my_size += 1 + ::protobuf::rt::compute_raw_varint32_size(len) + len;
                },
            };
        }
        my_size += ::protobuf::rt::unknown_fields_size(self.get_unknown_fields());
        self.cached_size.set(my_size);
        my_size
    }

    fn write_to_with_cached_sizes(&self, os: &mut ::protobuf::CodedOutputStream) -> ::protobuf::ProtobufResult<()> {
        if let ::std::option::Option::Some(ref v) = self.msg {
            match v {
                &ClientMsg_oneof_msg::setup(ref v) => {
                    os.write_tag(1, ::protobuf::wire_format::WireTypeLengthDelimited)?;
                    os.write_raw_varint32(v.get_cached_size())?;
                    v.write_to_with_cached_sizes(os)?;
                },
                &ClientMsg_oneof_msg::player_connect(ref v) => {
                    os.write_tag(2, ::protobuf::wire_format::WireTypeLengthDelimited)?;
                    os.write_raw_varint32(v.get_cached_size())?;
                    v.write_to_with_cached_sizes(os)?;
                },
                &ClientMsg_oneof_msg::player_disconnect(ref v) => {
                    os.write_tag(3, ::protobuf::wire_format::WireTypeLengthDelimited)?;
                    os.write_raw_varint32(v.get_cached_size())?;
                    v.write_to_with_cached_sizes(os)?;
                },
                &ClientMsg_oneof_msg::player_input(ref v) => {
                    os.write_tag(4, ::protobuf::wire_format::WireTypeLengthDelimited)?;
                    os.write_raw_varint32(v.get_cached_size())?;
                    v.write_to_with_cached_sizes(os)?;
                },
            };
        }
        os.write_unknown_fields(self.get_unknown_fields())?;
        ::std::result::Result::Ok(())
    }

    fn get_cached_size(&self) -> u32 {
        self.cached_size.get()
    }

    fn get_unknown_fields(&self) -> &::protobuf::UnknownFields {
        &self.unknown_fields
    }

    fn mut_unknown_fields(&mut self) -> &mut ::protobuf::UnknownFields {
        &mut self.unknown_fields
    }

    fn as_any(&self) -> &::std::any::Any {
        self as &::std::any::Any
    }
    fn as_any_mut(&mut self) -> &mut ::std::any::Any {
        self as &mut ::std::any::Any
    }
    fn into_any(self: Box<Self>) -> ::std::boxed::Box<::std::any::Any> {
        self
    }

    fn descriptor(&self) -> &'static ::protobuf::reflect::MessageDescriptor {
        ::protobuf::MessageStatic::descriptor_static(None::<Self>)
    }
}

impl ::protobuf::MessageStatic for ClientMsg {
    fn new() -> ClientMsg {
        ClientMsg::new()
    }

    fn descriptor_static(_: ::std::option::Option<ClientMsg>) -> &'static ::protobuf::reflect::MessageDescriptor {
        static mut descriptor: ::protobuf::lazy::Lazy<::protobuf::reflect::MessageDescriptor> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ::protobuf::reflect::MessageDescriptor,
        };
        unsafe {
            descriptor.get(|| {
                let mut fields = ::std::vec::Vec::new();
                fields.push(::protobuf::reflect::accessor::make_singular_message_accessor::<_, ClientMsg_Setup>(
                    "setup",
                    ClientMsg::has_setup,
                    ClientMsg::get_setup,
                ));
                fields.push(::protobuf::reflect::accessor::make_singular_message_accessor::<_, ClientMsg_PlayerConnect>(
                    "player_connect",
                    ClientMsg::has_player_connect,
                    ClientMsg::get_player_connect,
                ));
                fields.push(::protobuf::reflect::accessor::make_singular_message_accessor::<_, ClientMsg_PlayerDisconnect>(
                    "player_disconnect",
                    ClientMsg::has_player_disconnect,
                    ClientMsg::get_player_disconnect,
                ));
                fields.push(::protobuf::reflect::accessor::make_singular_message_accessor::<_, ClientMsg_PlayerInput>(
                    "player_input",
                    ClientMsg::has_player_input,
                    ClientMsg::get_player_input,
                ));
                ::protobuf::reflect::MessageDescriptor::new::<ClientMsg>(
                    "ClientMsg",
                    fields,
                    file_descriptor_proto()
                )
            })
        }
    }
}

impl ::protobuf::Clear for ClientMsg {
    fn clear(&mut self) {
        self.clear_setup();
        self.clear_player_connect();
        self.clear_player_disconnect();
        self.clear_player_input();
        self.unknown_fields.clear();
    }
}

impl ::std::fmt::Debug for ClientMsg {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::protobuf::text_format::fmt(self, f)
    }
}

impl ::protobuf::reflect::ProtobufValue for ClientMsg {
    fn as_ref(&self) -> ::protobuf::reflect::ProtobufValueRef {
        ::protobuf::reflect::ProtobufValueRef::Message(self)
    }
}

#[derive(PartialEq,Clone,Default)]
pub struct ClientMsg_Setup {
    // message fields
    pub host: ::std::string::String,
    // special fields
    unknown_fields: ::protobuf::UnknownFields,
    cached_size: ::protobuf::CachedSize,
}

// see codegen.rs for the explanation why impl Sync explicitly
unsafe impl ::std::marker::Sync for ClientMsg_Setup {}

impl ClientMsg_Setup {
    pub fn new() -> ClientMsg_Setup {
        ::std::default::Default::default()
    }

    pub fn default_instance() -> &'static ClientMsg_Setup {
        static mut instance: ::protobuf::lazy::Lazy<ClientMsg_Setup> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ClientMsg_Setup,
        };
        unsafe {
            instance.get(ClientMsg_Setup::new)
        }
    }

    // string host = 1;

    pub fn clear_host(&mut self) {
        self.host.clear();
    }

    // Param is passed by value, moved
    pub fn set_host(&mut self, v: ::std::string::String) {
        self.host = v;
    }

    // Mutable pointer to the field.
    // If field is not initialized, it is initialized with default value first.
    pub fn mut_host(&mut self) -> &mut ::std::string::String {
        &mut self.host
    }

    // Take field
    pub fn take_host(&mut self) -> ::std::string::String {
        ::std::mem::replace(&mut self.host, ::std::string::String::new())
    }

    pub fn get_host(&self) -> &str {
        &self.host
    }

    fn get_host_for_reflect(&self) -> &::std::string::String {
        &self.host
    }

    fn mut_host_for_reflect(&mut self) -> &mut ::std::string::String {
        &mut self.host
    }
}

impl ::protobuf::Message for ClientMsg_Setup {
    fn is_initialized(&self) -> bool {
        true
    }

    fn merge_from(&mut self, is: &mut ::protobuf::CodedInputStream) -> ::protobuf::ProtobufResult<()> {
        while !is.eof()? {
            let (field_number, wire_type) = is.read_tag_unpack()?;
            match field_number {
                1 => {
                    ::protobuf::rt::read_singular_proto3_string_into(wire_type, is, &mut self.host)?;
                },
                _ => {
                    ::protobuf::rt::read_unknown_or_skip_group(field_number, wire_type, is, self.mut_unknown_fields())?;
                },
            };
        }
        ::std::result::Result::Ok(())
    }

    // Compute sizes of nested messages
    #[allow(unused_variables)]
    fn compute_size(&self) -> u32 {
        let mut my_size = 0;
        if !self.host.is_empty() {
            my_size += ::protobuf::rt::string_size(1, &self.host);
        }
        my_size += ::protobuf::rt::unknown_fields_size(self.get_unknown_fields());
        self.cached_size.set(my_size);
        my_size
    }

    fn write_to_with_cached_sizes(&self, os: &mut ::protobuf::CodedOutputStream) -> ::protobuf::ProtobufResult<()> {
        if !self.host.is_empty() {
            os.write_string(1, &self.host)?;
        }
        os.write_unknown_fields(self.get_unknown_fields())?;
        ::std::result::Result::Ok(())
    }

    fn get_cached_size(&self) -> u32 {
        self.cached_size.get()
    }

    fn get_unknown_fields(&self) -> &::protobuf::UnknownFields {
        &self.unknown_fields
    }

    fn mut_unknown_fields(&mut self) -> &mut ::protobuf::UnknownFields {
        &mut self.unknown_fields
    }

    fn as_any(&self) -> &::std::any::Any {
        self as &::std::any::Any
    }
    fn as_any_mut(&mut self) -> &mut ::std::any::Any {
        self as &mut ::std::any::Any
    }
    fn into_any(self: Box<Self>) -> ::std::boxed::Box<::std::any::Any> {
        self
    }

    fn descriptor(&self) -> &'static ::protobuf::reflect::MessageDescriptor {
        ::protobuf::MessageStatic::descriptor_static(None::<Self>)
    }
}

impl ::protobuf::MessageStatic for ClientMsg_Setup {
    fn new() -> ClientMsg_Setup {
        ClientMsg_Setup::new()
    }

    fn descriptor_static(_: ::std::option::Option<ClientMsg_Setup>) -> &'static ::protobuf::reflect::MessageDescriptor {
        static mut descriptor: ::protobuf::lazy::Lazy<::protobuf::reflect::MessageDescriptor> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ::protobuf::reflect::MessageDescriptor,
        };
        unsafe {
            descriptor.get(|| {
                let mut fields = ::std::vec::Vec::new();
                fields.push(::protobuf::reflect::accessor::make_simple_field_accessor::<_, ::protobuf::types::ProtobufTypeString>(
                    "host",
                    ClientMsg_Setup::get_host_for_reflect,
                    ClientMsg_Setup::mut_host_for_reflect,
                ));
                ::protobuf::reflect::MessageDescriptor::new::<ClientMsg_Setup>(
                    "ClientMsg_Setup",
                    fields,
                    file_descriptor_proto()
                )
            })
        }
    }
}

impl ::protobuf::Clear for ClientMsg_Setup {
    fn clear(&mut self) {
        self.clear_host();
        self.unknown_fields.clear();
    }
}

impl ::std::fmt::Debug for ClientMsg_Setup {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::protobuf::text_format::fmt(self, f)
    }
}

impl ::protobuf::reflect::ProtobufValue for ClientMsg_Setup {
    fn as_ref(&self) -> ::protobuf::reflect::ProtobufValueRef {
        ::protobuf::reflect::ProtobufValueRef::Message(self)
    }
}

#[derive(PartialEq,Clone,Default)]
pub struct ClientMsg_PlayerConnect {
    // message fields
    pub ckey: ::std::string::String,
    pub ip: ::std::string::String,
    pub cid: ::std::string::String,
    // special fields
    unknown_fields: ::protobuf::UnknownFields,
    cached_size: ::protobuf::CachedSize,
}

// see codegen.rs for the explanation why impl Sync explicitly
unsafe impl ::std::marker::Sync for ClientMsg_PlayerConnect {}

impl ClientMsg_PlayerConnect {
    pub fn new() -> ClientMsg_PlayerConnect {
        ::std::default::Default::default()
    }

    pub fn default_instance() -> &'static ClientMsg_PlayerConnect {
        static mut instance: ::protobuf::lazy::Lazy<ClientMsg_PlayerConnect> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ClientMsg_PlayerConnect,
        };
        unsafe {
            instance.get(ClientMsg_PlayerConnect::new)
        }
    }

    // string ckey = 1;

    pub fn clear_ckey(&mut self) {
        self.ckey.clear();
    }

    // Param is passed by value, moved
    pub fn set_ckey(&mut self, v: ::std::string::String) {
        self.ckey = v;
    }

    // Mutable pointer to the field.
    // If field is not initialized, it is initialized with default value first.
    pub fn mut_ckey(&mut self) -> &mut ::std::string::String {
        &mut self.ckey
    }

    // Take field
    pub fn take_ckey(&mut self) -> ::std::string::String {
        ::std::mem::replace(&mut self.ckey, ::std::string::String::new())
    }

    pub fn get_ckey(&self) -> &str {
        &self.ckey
    }

    fn get_ckey_for_reflect(&self) -> &::std::string::String {
        &self.ckey
    }

    fn mut_ckey_for_reflect(&mut self) -> &mut ::std::string::String {
        &mut self.ckey
    }

    // string ip = 2;

    pub fn clear_ip(&mut self) {
        self.ip.clear();
    }

    // Param is passed by value, moved
    pub fn set_ip(&mut self, v: ::std::string::String) {
        self.ip = v;
    }

    // Mutable pointer to the field.
    // If field is not initialized, it is initialized with default value first.
    pub fn mut_ip(&mut self) -> &mut ::std::string::String {
        &mut self.ip
    }

    // Take field
    pub fn take_ip(&mut self) -> ::std::string::String {
        ::std::mem::replace(&mut self.ip, ::std::string::String::new())
    }

    pub fn get_ip(&self) -> &str {
        &self.ip
    }

    fn get_ip_for_reflect(&self) -> &::std::string::String {
        &self.ip
    }

    fn mut_ip_for_reflect(&mut self) -> &mut ::std::string::String {
        &mut self.ip
    }

    // string cid = 3;

    pub fn clear_cid(&mut self) {
        self.cid.clear();
    }

    // Param is passed by value, moved
    pub fn set_cid(&mut self, v: ::std::string::String) {
        self.cid = v;
    }

    // Mutable pointer to the field.
    // If field is not initialized, it is initialized with default value first.
    pub fn mut_cid(&mut self) -> &mut ::std::string::String {
        &mut self.cid
    }

    // Take field
    pub fn take_cid(&mut self) -> ::std::string::String {
        ::std::mem::replace(&mut self.cid, ::std::string::String::new())
    }

    pub fn get_cid(&self) -> &str {
        &self.cid
    }

    fn get_cid_for_reflect(&self) -> &::std::string::String {
        &self.cid
    }

    fn mut_cid_for_reflect(&mut self) -> &mut ::std::string::String {
        &mut self.cid
    }
}

impl ::protobuf::Message for ClientMsg_PlayerConnect {
    fn is_initialized(&self) -> bool {
        true
    }

    fn merge_from(&mut self, is: &mut ::protobuf::CodedInputStream) -> ::protobuf::ProtobufResult<()> {
        while !is.eof()? {
            let (field_number, wire_type) = is.read_tag_unpack()?;
            match field_number {
                1 => {
                    ::protobuf::rt::read_singular_proto3_string_into(wire_type, is, &mut self.ckey)?;
                },
                2 => {
                    ::protobuf::rt::read_singular_proto3_string_into(wire_type, is, &mut self.ip)?;
                },
                3 => {
                    ::protobuf::rt::read_singular_proto3_string_into(wire_type, is, &mut self.cid)?;
                },
                _ => {
                    ::protobuf::rt::read_unknown_or_skip_group(field_number, wire_type, is, self.mut_unknown_fields())?;
                },
            };
        }
        ::std::result::Result::Ok(())
    }

    // Compute sizes of nested messages
    #[allow(unused_variables)]
    fn compute_size(&self) -> u32 {
        let mut my_size = 0;
        if !self.ckey.is_empty() {
            my_size += ::protobuf::rt::string_size(1, &self.ckey);
        }
        if !self.ip.is_empty() {
            my_size += ::protobuf::rt::string_size(2, &self.ip);
        }
        if !self.cid.is_empty() {
            my_size += ::protobuf::rt::string_size(3, &self.cid);
        }
        my_size += ::protobuf::rt::unknown_fields_size(self.get_unknown_fields());
        self.cached_size.set(my_size);
        my_size
    }

    fn write_to_with_cached_sizes(&self, os: &mut ::protobuf::CodedOutputStream) -> ::protobuf::ProtobufResult<()> {
        if !self.ckey.is_empty() {
            os.write_string(1, &self.ckey)?;
        }
        if !self.ip.is_empty() {
            os.write_string(2, &self.ip)?;
        }
        if !self.cid.is_empty() {
            os.write_string(3, &self.cid)?;
        }
        os.write_unknown_fields(self.get_unknown_fields())?;
        ::std::result::Result::Ok(())
    }

    fn get_cached_size(&self) -> u32 {
        self.cached_size.get()
    }

    fn get_unknown_fields(&self) -> &::protobuf::UnknownFields {
        &self.unknown_fields
    }

    fn mut_unknown_fields(&mut self) -> &mut ::protobuf::UnknownFields {
        &mut self.unknown_fields
    }

    fn as_any(&self) -> &::std::any::Any {
        self as &::std::any::Any
    }
    fn as_any_mut(&mut self) -> &mut ::std::any::Any {
        self as &mut ::std::any::Any
    }
    fn into_any(self: Box<Self>) -> ::std::boxed::Box<::std::any::Any> {
        self
    }

    fn descriptor(&self) -> &'static ::protobuf::reflect::MessageDescriptor {
        ::protobuf::MessageStatic::descriptor_static(None::<Self>)
    }
}

impl ::protobuf::MessageStatic for ClientMsg_PlayerConnect {
    fn new() -> ClientMsg_PlayerConnect {
        ClientMsg_PlayerConnect::new()
    }

    fn descriptor_static(_: ::std::option::Option<ClientMsg_PlayerConnect>) -> &'static ::protobuf::reflect::MessageDescriptor {
        static mut descriptor: ::protobuf::lazy::Lazy<::protobuf::reflect::MessageDescriptor> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ::protobuf::reflect::MessageDescriptor,
        };
        unsafe {
            descriptor.get(|| {
                let mut fields = ::std::vec::Vec::new();
                fields.push(::protobuf::reflect::accessor::make_simple_field_accessor::<_, ::protobuf::types::ProtobufTypeString>(
                    "ckey",
                    ClientMsg_PlayerConnect::get_ckey_for_reflect,
                    ClientMsg_PlayerConnect::mut_ckey_for_reflect,
                ));
                fields.push(::protobuf::reflect::accessor::make_simple_field_accessor::<_, ::protobuf::types::ProtobufTypeString>(
                    "ip",
                    ClientMsg_PlayerConnect::get_ip_for_reflect,
                    ClientMsg_PlayerConnect::mut_ip_for_reflect,
                ));
                fields.push(::protobuf::reflect::accessor::make_simple_field_accessor::<_, ::protobuf::types::ProtobufTypeString>(
                    "cid",
                    ClientMsg_PlayerConnect::get_cid_for_reflect,
                    ClientMsg_PlayerConnect::mut_cid_for_reflect,
                ));
                ::protobuf::reflect::MessageDescriptor::new::<ClientMsg_PlayerConnect>(
                    "ClientMsg_PlayerConnect",
                    fields,
                    file_descriptor_proto()
                )
            })
        }
    }
}

impl ::protobuf::Clear for ClientMsg_PlayerConnect {
    fn clear(&mut self) {
        self.clear_ckey();
        self.clear_ip();
        self.clear_cid();
        self.unknown_fields.clear();
    }
}

impl ::std::fmt::Debug for ClientMsg_PlayerConnect {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::protobuf::text_format::fmt(self, f)
    }
}

impl ::protobuf::reflect::ProtobufValue for ClientMsg_PlayerConnect {
    fn as_ref(&self) -> ::protobuf::reflect::ProtobufValueRef {
        ::protobuf::reflect::ProtobufValueRef::Message(self)
    }
}

#[derive(PartialEq,Clone,Default)]
pub struct ClientMsg_PlayerDisconnect {
    // message fields
    pub ckey: ::std::string::String,
    // special fields
    unknown_fields: ::protobuf::UnknownFields,
    cached_size: ::protobuf::CachedSize,
}

// see codegen.rs for the explanation why impl Sync explicitly
unsafe impl ::std::marker::Sync for ClientMsg_PlayerDisconnect {}

impl ClientMsg_PlayerDisconnect {
    pub fn new() -> ClientMsg_PlayerDisconnect {
        ::std::default::Default::default()
    }

    pub fn default_instance() -> &'static ClientMsg_PlayerDisconnect {
        static mut instance: ::protobuf::lazy::Lazy<ClientMsg_PlayerDisconnect> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ClientMsg_PlayerDisconnect,
        };
        unsafe {
            instance.get(ClientMsg_PlayerDisconnect::new)
        }
    }

    // string ckey = 1;

    pub fn clear_ckey(&mut self) {
        self.ckey.clear();
    }

    // Param is passed by value, moved
    pub fn set_ckey(&mut self, v: ::std::string::String) {
        self.ckey = v;
    }

    // Mutable pointer to the field.
    // If field is not initialized, it is initialized with default value first.
    pub fn mut_ckey(&mut self) -> &mut ::std::string::String {
        &mut self.ckey
    }

    // Take field
    pub fn take_ckey(&mut self) -> ::std::string::String {
        ::std::mem::replace(&mut self.ckey, ::std::string::String::new())
    }

    pub fn get_ckey(&self) -> &str {
        &self.ckey
    }

    fn get_ckey_for_reflect(&self) -> &::std::string::String {
        &self.ckey
    }

    fn mut_ckey_for_reflect(&mut self) -> &mut ::std::string::String {
        &mut self.ckey
    }
}

impl ::protobuf::Message for ClientMsg_PlayerDisconnect {
    fn is_initialized(&self) -> bool {
        true
    }

    fn merge_from(&mut self, is: &mut ::protobuf::CodedInputStream) -> ::protobuf::ProtobufResult<()> {
        while !is.eof()? {
            let (field_number, wire_type) = is.read_tag_unpack()?;
            match field_number {
                1 => {
                    ::protobuf::rt::read_singular_proto3_string_into(wire_type, is, &mut self.ckey)?;
                },
                _ => {
                    ::protobuf::rt::read_unknown_or_skip_group(field_number, wire_type, is, self.mut_unknown_fields())?;
                },
            };
        }
        ::std::result::Result::Ok(())
    }

    // Compute sizes of nested messages
    #[allow(unused_variables)]
    fn compute_size(&self) -> u32 {
        let mut my_size = 0;
        if !self.ckey.is_empty() {
            my_size += ::protobuf::rt::string_size(1, &self.ckey);
        }
        my_size += ::protobuf::rt::unknown_fields_size(self.get_unknown_fields());
        self.cached_size.set(my_size);
        my_size
    }

    fn write_to_with_cached_sizes(&self, os: &mut ::protobuf::CodedOutputStream) -> ::protobuf::ProtobufResult<()> {
        if !self.ckey.is_empty() {
            os.write_string(1, &self.ckey)?;
        }
        os.write_unknown_fields(self.get_unknown_fields())?;
        ::std::result::Result::Ok(())
    }

    fn get_cached_size(&self) -> u32 {
        self.cached_size.get()
    }

    fn get_unknown_fields(&self) -> &::protobuf::UnknownFields {
        &self.unknown_fields
    }

    fn mut_unknown_fields(&mut self) -> &mut ::protobuf::UnknownFields {
        &mut self.unknown_fields
    }

    fn as_any(&self) -> &::std::any::Any {
        self as &::std::any::Any
    }
    fn as_any_mut(&mut self) -> &mut ::std::any::Any {
        self as &mut ::std::any::Any
    }
    fn into_any(self: Box<Self>) -> ::std::boxed::Box<::std::any::Any> {
        self
    }

    fn descriptor(&self) -> &'static ::protobuf::reflect::MessageDescriptor {
        ::protobuf::MessageStatic::descriptor_static(None::<Self>)
    }
}

impl ::protobuf::MessageStatic for ClientMsg_PlayerDisconnect {
    fn new() -> ClientMsg_PlayerDisconnect {
        ClientMsg_PlayerDisconnect::new()
    }

    fn descriptor_static(_: ::std::option::Option<ClientMsg_PlayerDisconnect>) -> &'static ::protobuf::reflect::MessageDescriptor {
        static mut descriptor: ::protobuf::lazy::Lazy<::protobuf::reflect::MessageDescriptor> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ::protobuf::reflect::MessageDescriptor,
        };
        unsafe {
            descriptor.get(|| {
                let mut fields = ::std::vec::Vec::new();
                fields.push(::protobuf::reflect::accessor::make_simple_field_accessor::<_, ::protobuf::types::ProtobufTypeString>(
                    "ckey",
                    ClientMsg_PlayerDisconnect::get_ckey_for_reflect,
                    ClientMsg_PlayerDisconnect::mut_ckey_for_reflect,
                ));
                ::protobuf::reflect::MessageDescriptor::new::<ClientMsg_PlayerDisconnect>(
                    "ClientMsg_PlayerDisconnect",
                    fields,
                    file_descriptor_proto()
                )
            })
        }
    }
}

impl ::protobuf::Clear for ClientMsg_PlayerDisconnect {
    fn clear(&mut self) {
        self.clear_ckey();
        self.unknown_fields.clear();
    }
}

impl ::std::fmt::Debug for ClientMsg_PlayerDisconnect {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::protobuf::text_format::fmt(self, f)
    }
}

impl ::protobuf::reflect::ProtobufValue for ClientMsg_PlayerDisconnect {
    fn as_ref(&self) -> ::protobuf::reflect::ProtobufValueRef {
        ::protobuf::reflect::ProtobufValueRef::Message(self)
    }
}

#[derive(PartialEq,Clone,Default)]
pub struct ClientMsg_PlayerInput {
    // message fields
    pub ckey: ::std::string::String,
    // message oneof groups
    event: ::std::option::Option<ClientMsg_PlayerInput_oneof_event>,
    // special fields
    unknown_fields: ::protobuf::UnknownFields,
    cached_size: ::protobuf::CachedSize,
}

// see codegen.rs for the explanation why impl Sync explicitly
unsafe impl ::std::marker::Sync for ClientMsg_PlayerInput {}

#[derive(Clone,PartialEq)]
pub enum ClientMsg_PlayerInput_oneof_event {
    movement(Direction),
    click(ClientMsg_PlayerInput_Click),
    verb(ClientMsg_PlayerInput_Verb),
}

impl ClientMsg_PlayerInput {
    pub fn new() -> ClientMsg_PlayerInput {
        ::std::default::Default::default()
    }

    pub fn default_instance() -> &'static ClientMsg_PlayerInput {
        static mut instance: ::protobuf::lazy::Lazy<ClientMsg_PlayerInput> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ClientMsg_PlayerInput,
        };
        unsafe {
            instance.get(ClientMsg_PlayerInput::new)
        }
    }

    // string ckey = 1;

    pub fn clear_ckey(&mut self) {
        self.ckey.clear();
    }

    // Param is passed by value, moved
    pub fn set_ckey(&mut self, v: ::std::string::String) {
        self.ckey = v;
    }

    // Mutable pointer to the field.
    // If field is not initialized, it is initialized with default value first.
    pub fn mut_ckey(&mut self) -> &mut ::std::string::String {
        &mut self.ckey
    }

    // Take field
    pub fn take_ckey(&mut self) -> ::std::string::String {
        ::std::mem::replace(&mut self.ckey, ::std::string::String::new())
    }

    pub fn get_ckey(&self) -> &str {
        &self.ckey
    }

    fn get_ckey_for_reflect(&self) -> &::std::string::String {
        &self.ckey
    }

    fn mut_ckey_for_reflect(&mut self) -> &mut ::std::string::String {
        &mut self.ckey
    }

    // .Direction movement = 2;

    pub fn clear_movement(&mut self) {
        self.event = ::std::option::Option::None;
    }

    pub fn has_movement(&self) -> bool {
        match self.event {
            ::std::option::Option::Some(ClientMsg_PlayerInput_oneof_event::movement(..)) => true,
            _ => false,
        }
    }

    // Param is passed by value, moved
    pub fn set_movement(&mut self, v: Direction) {
        self.event = ::std::option::Option::Some(ClientMsg_PlayerInput_oneof_event::movement(v))
    }

    pub fn get_movement(&self) -> Direction {
        match self.event {
            ::std::option::Option::Some(ClientMsg_PlayerInput_oneof_event::movement(v)) => v,
            _ => Direction::UNKNOWN,
        }
    }

    // .ClientMsg.PlayerInput.Click click = 3;

    pub fn clear_click(&mut self) {
        self.event = ::std::option::Option::None;
    }

    pub fn has_click(&self) -> bool {
        match self.event {
            ::std::option::Option::Some(ClientMsg_PlayerInput_oneof_event::click(..)) => true,
            _ => false,
        }
    }

    // Param is passed by value, moved
    pub fn set_click(&mut self, v: ClientMsg_PlayerInput_Click) {
        self.event = ::std::option::Option::Some(ClientMsg_PlayerInput_oneof_event::click(v))
    }

    // Mutable pointer to the field.
    pub fn mut_click(&mut self) -> &mut ClientMsg_PlayerInput_Click {
        if let ::std::option::Option::Some(ClientMsg_PlayerInput_oneof_event::click(_)) = self.event {
        } else {
            self.event = ::std::option::Option::Some(ClientMsg_PlayerInput_oneof_event::click(ClientMsg_PlayerInput_Click::new()));
        }
        match self.event {
            ::std::option::Option::Some(ClientMsg_PlayerInput_oneof_event::click(ref mut v)) => v,
            _ => panic!(),
        }
    }

    // Take field
    pub fn take_click(&mut self) -> ClientMsg_PlayerInput_Click {
        if self.has_click() {
            match self.event.take() {
                ::std::option::Option::Some(ClientMsg_PlayerInput_oneof_event::click(v)) => v,
                _ => panic!(),
            }
        } else {
            ClientMsg_PlayerInput_Click::new()
        }
    }

    pub fn get_click(&self) -> &ClientMsg_PlayerInput_Click {
        match self.event {
            ::std::option::Option::Some(ClientMsg_PlayerInput_oneof_event::click(ref v)) => v,
            _ => ClientMsg_PlayerInput_Click::default_instance(),
        }
    }

    // .ClientMsg.PlayerInput.Verb verb = 4;

    pub fn clear_verb(&mut self) {
        self.event = ::std::option::Option::None;
    }

    pub fn has_verb(&self) -> bool {
        match self.event {
            ::std::option::Option::Some(ClientMsg_PlayerInput_oneof_event::verb(..)) => true,
            _ => false,
        }
    }

    // Param is passed by value, moved
    pub fn set_verb(&mut self, v: ClientMsg_PlayerInput_Verb) {
        self.event = ::std::option::Option::Some(ClientMsg_PlayerInput_oneof_event::verb(v))
    }

    // Mutable pointer to the field.
    pub fn mut_verb(&mut self) -> &mut ClientMsg_PlayerInput_Verb {
        if let ::std::option::Option::Some(ClientMsg_PlayerInput_oneof_event::verb(_)) = self.event {
        } else {
            self.event = ::std::option::Option::Some(ClientMsg_PlayerInput_oneof_event::verb(ClientMsg_PlayerInput_Verb::new()));
        }
        match self.event {
            ::std::option::Option::Some(ClientMsg_PlayerInput_oneof_event::verb(ref mut v)) => v,
            _ => panic!(),
        }
    }

    // Take field
    pub fn take_verb(&mut self) -> ClientMsg_PlayerInput_Verb {
        if self.has_verb() {
            match self.event.take() {
                ::std::option::Option::Some(ClientMsg_PlayerInput_oneof_event::verb(v)) => v,
                _ => panic!(),
            }
        } else {
            ClientMsg_PlayerInput_Verb::new()
        }
    }

    pub fn get_verb(&self) -> &ClientMsg_PlayerInput_Verb {
        match self.event {
            ::std::option::Option::Some(ClientMsg_PlayerInput_oneof_event::verb(ref v)) => v,
            _ => ClientMsg_PlayerInput_Verb::default_instance(),
        }
    }
}

impl ::protobuf::Message for ClientMsg_PlayerInput {
    fn is_initialized(&self) -> bool {
        if let Some(ClientMsg_PlayerInput_oneof_event::click(ref v)) = self.event {
            if !v.is_initialized() {
                return false;
            }
        }
        if let Some(ClientMsg_PlayerInput_oneof_event::verb(ref v)) = self.event {
            if !v.is_initialized() {
                return false;
            }
        }
        true
    }

    fn merge_from(&mut self, is: &mut ::protobuf::CodedInputStream) -> ::protobuf::ProtobufResult<()> {
        while !is.eof()? {
            let (field_number, wire_type) = is.read_tag_unpack()?;
            match field_number {
                1 => {
                    ::protobuf::rt::read_singular_proto3_string_into(wire_type, is, &mut self.ckey)?;
                },
                2 => {
                    if wire_type != ::protobuf::wire_format::WireTypeVarint {
                        return ::std::result::Result::Err(::protobuf::rt::unexpected_wire_type(wire_type));
                    }
                    self.event = ::std::option::Option::Some(ClientMsg_PlayerInput_oneof_event::movement(is.read_enum()?));
                },
                3 => {
                    if wire_type != ::protobuf::wire_format::WireTypeLengthDelimited {
                        return ::std::result::Result::Err(::protobuf::rt::unexpected_wire_type(wire_type));
                    }
                    self.event = ::std::option::Option::Some(ClientMsg_PlayerInput_oneof_event::click(is.read_message()?));
                },
                4 => {
                    if wire_type != ::protobuf::wire_format::WireTypeLengthDelimited {
                        return ::std::result::Result::Err(::protobuf::rt::unexpected_wire_type(wire_type));
                    }
                    self.event = ::std::option::Option::Some(ClientMsg_PlayerInput_oneof_event::verb(is.read_message()?));
                },
                _ => {
                    ::protobuf::rt::read_unknown_or_skip_group(field_number, wire_type, is, self.mut_unknown_fields())?;
                },
            };
        }
        ::std::result::Result::Ok(())
    }

    // Compute sizes of nested messages
    #[allow(unused_variables)]
    fn compute_size(&self) -> u32 {
        let mut my_size = 0;
        if !self.ckey.is_empty() {
            my_size += ::protobuf::rt::string_size(1, &self.ckey);
        }
        if let ::std::option::Option::Some(ref v) = self.event {
            match v {
                &ClientMsg_PlayerInput_oneof_event::movement(v) => {
                    my_size += ::protobuf::rt::enum_size(2, v);
                },
                &ClientMsg_PlayerInput_oneof_event::click(ref v) => {
                    let len = v.compute_size();
                    my_size += 1 + ::protobuf::rt::compute_raw_varint32_size(len) + len;
                },
                &ClientMsg_PlayerInput_oneof_event::verb(ref v) => {
                    let len = v.compute_size();
                    my_size += 1 + ::protobuf::rt::compute_raw_varint32_size(len) + len;
                },
            };
        }
        my_size += ::protobuf::rt::unknown_fields_size(self.get_unknown_fields());
        self.cached_size.set(my_size);
        my_size
    }

    fn write_to_with_cached_sizes(&self, os: &mut ::protobuf::CodedOutputStream) -> ::protobuf::ProtobufResult<()> {
        if !self.ckey.is_empty() {
            os.write_string(1, &self.ckey)?;
        }
        if let ::std::option::Option::Some(ref v) = self.event {
            match v {
                &ClientMsg_PlayerInput_oneof_event::movement(v) => {
                    os.write_enum(2, v.value())?;
                },
                &ClientMsg_PlayerInput_oneof_event::click(ref v) => {
                    os.write_tag(3, ::protobuf::wire_format::WireTypeLengthDelimited)?;
                    os.write_raw_varint32(v.get_cached_size())?;
                    v.write_to_with_cached_sizes(os)?;
                },
                &ClientMsg_PlayerInput_oneof_event::verb(ref v) => {
                    os.write_tag(4, ::protobuf::wire_format::WireTypeLengthDelimited)?;
                    os.write_raw_varint32(v.get_cached_size())?;
                    v.write_to_with_cached_sizes(os)?;
                },
            };
        }
        os.write_unknown_fields(self.get_unknown_fields())?;
        ::std::result::Result::Ok(())
    }

    fn get_cached_size(&self) -> u32 {
        self.cached_size.get()
    }

    fn get_unknown_fields(&self) -> &::protobuf::UnknownFields {
        &self.unknown_fields
    }

    fn mut_unknown_fields(&mut self) -> &mut ::protobuf::UnknownFields {
        &mut self.unknown_fields
    }

    fn as_any(&self) -> &::std::any::Any {
        self as &::std::any::Any
    }
    fn as_any_mut(&mut self) -> &mut ::std::any::Any {
        self as &mut ::std::any::Any
    }
    fn into_any(self: Box<Self>) -> ::std::boxed::Box<::std::any::Any> {
        self
    }

    fn descriptor(&self) -> &'static ::protobuf::reflect::MessageDescriptor {
        ::protobuf::MessageStatic::descriptor_static(None::<Self>)
    }
}

impl ::protobuf::MessageStatic for ClientMsg_PlayerInput {
    fn new() -> ClientMsg_PlayerInput {
        ClientMsg_PlayerInput::new()
    }

    fn descriptor_static(_: ::std::option::Option<ClientMsg_PlayerInput>) -> &'static ::protobuf::reflect::MessageDescriptor {
        static mut descriptor: ::protobuf::lazy::Lazy<::protobuf::reflect::MessageDescriptor> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ::protobuf::reflect::MessageDescriptor,
        };
        unsafe {
            descriptor.get(|| {
                let mut fields = ::std::vec::Vec::new();
                fields.push(::protobuf::reflect::accessor::make_simple_field_accessor::<_, ::protobuf::types::ProtobufTypeString>(
                    "ckey",
                    ClientMsg_PlayerInput::get_ckey_for_reflect,
                    ClientMsg_PlayerInput::mut_ckey_for_reflect,
                ));
                fields.push(::protobuf::reflect::accessor::make_singular_enum_accessor::<_, Direction>(
                    "movement",
                    ClientMsg_PlayerInput::has_movement,
                    ClientMsg_PlayerInput::get_movement,
                ));
                fields.push(::protobuf::reflect::accessor::make_singular_message_accessor::<_, ClientMsg_PlayerInput_Click>(
                    "click",
                    ClientMsg_PlayerInput::has_click,
                    ClientMsg_PlayerInput::get_click,
                ));
                fields.push(::protobuf::reflect::accessor::make_singular_message_accessor::<_, ClientMsg_PlayerInput_Verb>(
                    "verb",
                    ClientMsg_PlayerInput::has_verb,
                    ClientMsg_PlayerInput::get_verb,
                ));
                ::protobuf::reflect::MessageDescriptor::new::<ClientMsg_PlayerInput>(
                    "ClientMsg_PlayerInput",
                    fields,
                    file_descriptor_proto()
                )
            })
        }
    }
}

impl ::protobuf::Clear for ClientMsg_PlayerInput {
    fn clear(&mut self) {
        self.clear_ckey();
        self.clear_movement();
        self.clear_click();
        self.clear_verb();
        self.unknown_fields.clear();
    }
}

impl ::std::fmt::Debug for ClientMsg_PlayerInput {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::protobuf::text_format::fmt(self, f)
    }
}

impl ::protobuf::reflect::ProtobufValue for ClientMsg_PlayerInput {
    fn as_ref(&self) -> ::protobuf::reflect::ProtobufValueRef {
        ::protobuf::reflect::ProtobufValueRef::Message(self)
    }
}

#[derive(PartialEq,Clone,Default)]
pub struct ClientMsg_PlayerInput_Click {
    // message fields
    pub target: i64,
    pub offset: ::protobuf::SingularPtrField<Vector>,
    // special fields
    unknown_fields: ::protobuf::UnknownFields,
    cached_size: ::protobuf::CachedSize,
}

// see codegen.rs for the explanation why impl Sync explicitly
unsafe impl ::std::marker::Sync for ClientMsg_PlayerInput_Click {}

impl ClientMsg_PlayerInput_Click {
    pub fn new() -> ClientMsg_PlayerInput_Click {
        ::std::default::Default::default()
    }

    pub fn default_instance() -> &'static ClientMsg_PlayerInput_Click {
        static mut instance: ::protobuf::lazy::Lazy<ClientMsg_PlayerInput_Click> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ClientMsg_PlayerInput_Click,
        };
        unsafe {
            instance.get(ClientMsg_PlayerInput_Click::new)
        }
    }

    // int64 target = 1;

    pub fn clear_target(&mut self) {
        self.target = 0;
    }

    // Param is passed by value, moved
    pub fn set_target(&mut self, v: i64) {
        self.target = v;
    }

    pub fn get_target(&self) -> i64 {
        self.target
    }

    fn get_target_for_reflect(&self) -> &i64 {
        &self.target
    }

    fn mut_target_for_reflect(&mut self) -> &mut i64 {
        &mut self.target
    }

    // .Vector offset = 2;

    pub fn clear_offset(&mut self) {
        self.offset.clear();
    }

    pub fn has_offset(&self) -> bool {
        self.offset.is_some()
    }

    // Param is passed by value, moved
    pub fn set_offset(&mut self, v: Vector) {
        self.offset = ::protobuf::SingularPtrField::some(v);
    }

    // Mutable pointer to the field.
    // If field is not initialized, it is initialized with default value first.
    pub fn mut_offset(&mut self) -> &mut Vector {
        if self.offset.is_none() {
            self.offset.set_default();
        }
        self.offset.as_mut().unwrap()
    }

    // Take field
    pub fn take_offset(&mut self) -> Vector {
        self.offset.take().unwrap_or_else(|| Vector::new())
    }

    pub fn get_offset(&self) -> &Vector {
        self.offset.as_ref().unwrap_or_else(|| Vector::default_instance())
    }

    fn get_offset_for_reflect(&self) -> &::protobuf::SingularPtrField<Vector> {
        &self.offset
    }

    fn mut_offset_for_reflect(&mut self) -> &mut ::protobuf::SingularPtrField<Vector> {
        &mut self.offset
    }
}

impl ::protobuf::Message for ClientMsg_PlayerInput_Click {
    fn is_initialized(&self) -> bool {
        for v in &self.offset {
            if !v.is_initialized() {
                return false;
            }
        };
        true
    }

    fn merge_from(&mut self, is: &mut ::protobuf::CodedInputStream) -> ::protobuf::ProtobufResult<()> {
        while !is.eof()? {
            let (field_number, wire_type) = is.read_tag_unpack()?;
            match field_number {
                1 => {
                    if wire_type != ::protobuf::wire_format::WireTypeVarint {
                        return ::std::result::Result::Err(::protobuf::rt::unexpected_wire_type(wire_type));
                    }
                    let tmp = is.read_int64()?;
                    self.target = tmp;
                },
                2 => {
                    ::protobuf::rt::read_singular_message_into(wire_type, is, &mut self.offset)?;
                },
                _ => {
                    ::protobuf::rt::read_unknown_or_skip_group(field_number, wire_type, is, self.mut_unknown_fields())?;
                },
            };
        }
        ::std::result::Result::Ok(())
    }

    // Compute sizes of nested messages
    #[allow(unused_variables)]
    fn compute_size(&self) -> u32 {
        let mut my_size = 0;
        if self.target != 0 {
            my_size += ::protobuf::rt::value_size(1, self.target, ::protobuf::wire_format::WireTypeVarint);
        }
        if let Some(ref v) = self.offset.as_ref() {
            let len = v.compute_size();
            my_size += 1 + ::protobuf::rt::compute_raw_varint32_size(len) + len;
        }
        my_size += ::protobuf::rt::unknown_fields_size(self.get_unknown_fields());
        self.cached_size.set(my_size);
        my_size
    }

    fn write_to_with_cached_sizes(&self, os: &mut ::protobuf::CodedOutputStream) -> ::protobuf::ProtobufResult<()> {
        if self.target != 0 {
            os.write_int64(1, self.target)?;
        }
        if let Some(ref v) = self.offset.as_ref() {
            os.write_tag(2, ::protobuf::wire_format::WireTypeLengthDelimited)?;
            os.write_raw_varint32(v.get_cached_size())?;
            v.write_to_with_cached_sizes(os)?;
        }
        os.write_unknown_fields(self.get_unknown_fields())?;
        ::std::result::Result::Ok(())
    }

    fn get_cached_size(&self) -> u32 {
        self.cached_size.get()
    }

    fn get_unknown_fields(&self) -> &::protobuf::UnknownFields {
        &self.unknown_fields
    }

    fn mut_unknown_fields(&mut self) -> &mut ::protobuf::UnknownFields {
        &mut self.unknown_fields
    }

    fn as_any(&self) -> &::std::any::Any {
        self as &::std::any::Any
    }
    fn as_any_mut(&mut self) -> &mut ::std::any::Any {
        self as &mut ::std::any::Any
    }
    fn into_any(self: Box<Self>) -> ::std::boxed::Box<::std::any::Any> {
        self
    }

    fn descriptor(&self) -> &'static ::protobuf::reflect::MessageDescriptor {
        ::protobuf::MessageStatic::descriptor_static(None::<Self>)
    }
}

impl ::protobuf::MessageStatic for ClientMsg_PlayerInput_Click {
    fn new() -> ClientMsg_PlayerInput_Click {
        ClientMsg_PlayerInput_Click::new()
    }

    fn descriptor_static(_: ::std::option::Option<ClientMsg_PlayerInput_Click>) -> &'static ::protobuf::reflect::MessageDescriptor {
        static mut descriptor: ::protobuf::lazy::Lazy<::protobuf::reflect::MessageDescriptor> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ::protobuf::reflect::MessageDescriptor,
        };
        unsafe {
            descriptor.get(|| {
                let mut fields = ::std::vec::Vec::new();
                fields.push(::protobuf::reflect::accessor::make_simple_field_accessor::<_, ::protobuf::types::ProtobufTypeInt64>(
                    "target",
                    ClientMsg_PlayerInput_Click::get_target_for_reflect,
                    ClientMsg_PlayerInput_Click::mut_target_for_reflect,
                ));
                fields.push(::protobuf::reflect::accessor::make_singular_ptr_field_accessor::<_, ::protobuf::types::ProtobufTypeMessage<Vector>>(
                    "offset",
                    ClientMsg_PlayerInput_Click::get_offset_for_reflect,
                    ClientMsg_PlayerInput_Click::mut_offset_for_reflect,
                ));
                ::protobuf::reflect::MessageDescriptor::new::<ClientMsg_PlayerInput_Click>(
                    "ClientMsg_PlayerInput_Click",
                    fields,
                    file_descriptor_proto()
                )
            })
        }
    }
}

impl ::protobuf::Clear for ClientMsg_PlayerInput_Click {
    fn clear(&mut self) {
        self.clear_target();
        self.clear_offset();
        self.unknown_fields.clear();
    }
}

impl ::std::fmt::Debug for ClientMsg_PlayerInput_Click {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::protobuf::text_format::fmt(self, f)
    }
}

impl ::protobuf::reflect::ProtobufValue for ClientMsg_PlayerInput_Click {
    fn as_ref(&self) -> ::protobuf::reflect::ProtobufValueRef {
        ::protobuf::reflect::ProtobufValueRef::Message(self)
    }
}

#[derive(PartialEq,Clone,Default)]
pub struct ClientMsg_PlayerInput_Verb {
    // message fields
    pub name: ::std::string::String,
    pub args: ::protobuf::RepeatedField<::std::string::String>,
    // special fields
    unknown_fields: ::protobuf::UnknownFields,
    cached_size: ::protobuf::CachedSize,
}

// see codegen.rs for the explanation why impl Sync explicitly
unsafe impl ::std::marker::Sync for ClientMsg_PlayerInput_Verb {}

impl ClientMsg_PlayerInput_Verb {
    pub fn new() -> ClientMsg_PlayerInput_Verb {
        ::std::default::Default::default()
    }

    pub fn default_instance() -> &'static ClientMsg_PlayerInput_Verb {
        static mut instance: ::protobuf::lazy::Lazy<ClientMsg_PlayerInput_Verb> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ClientMsg_PlayerInput_Verb,
        };
        unsafe {
            instance.get(ClientMsg_PlayerInput_Verb::new)
        }
    }

    // string name = 1;

    pub fn clear_name(&mut self) {
        self.name.clear();
    }

    // Param is passed by value, moved
    pub fn set_name(&mut self, v: ::std::string::String) {
        self.name = v;
    }

    // Mutable pointer to the field.
    // If field is not initialized, it is initialized with default value first.
    pub fn mut_name(&mut self) -> &mut ::std::string::String {
        &mut self.name
    }

    // Take field
    pub fn take_name(&mut self) -> ::std::string::String {
        ::std::mem::replace(&mut self.name, ::std::string::String::new())
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    fn get_name_for_reflect(&self) -> &::std::string::String {
        &self.name
    }

    fn mut_name_for_reflect(&mut self) -> &mut ::std::string::String {
        &mut self.name
    }

    // repeated string args = 2;

    pub fn clear_args(&mut self) {
        self.args.clear();
    }

    // Param is passed by value, moved
    pub fn set_args(&mut self, v: ::protobuf::RepeatedField<::std::string::String>) {
        self.args = v;
    }

    // Mutable pointer to the field.
    pub fn mut_args(&mut self) -> &mut ::protobuf::RepeatedField<::std::string::String> {
        &mut self.args
    }

    // Take field
    pub fn take_args(&mut self) -> ::protobuf::RepeatedField<::std::string::String> {
        ::std::mem::replace(&mut self.args, ::protobuf::RepeatedField::new())
    }

    pub fn get_args(&self) -> &[::std::string::String] {
        &self.args
    }

    fn get_args_for_reflect(&self) -> &::protobuf::RepeatedField<::std::string::String> {
        &self.args
    }

    fn mut_args_for_reflect(&mut self) -> &mut ::protobuf::RepeatedField<::std::string::String> {
        &mut self.args
    }
}

impl ::protobuf::Message for ClientMsg_PlayerInput_Verb {
    fn is_initialized(&self) -> bool {
        true
    }

    fn merge_from(&mut self, is: &mut ::protobuf::CodedInputStream) -> ::protobuf::ProtobufResult<()> {
        while !is.eof()? {
            let (field_number, wire_type) = is.read_tag_unpack()?;
            match field_number {
                1 => {
                    ::protobuf::rt::read_singular_proto3_string_into(wire_type, is, &mut self.name)?;
                },
                2 => {
                    ::protobuf::rt::read_repeated_string_into(wire_type, is, &mut self.args)?;
                },
                _ => {
                    ::protobuf::rt::read_unknown_or_skip_group(field_number, wire_type, is, self.mut_unknown_fields())?;
                },
            };
        }
        ::std::result::Result::Ok(())
    }

    // Compute sizes of nested messages
    #[allow(unused_variables)]
    fn compute_size(&self) -> u32 {
        let mut my_size = 0;
        if !self.name.is_empty() {
            my_size += ::protobuf::rt::string_size(1, &self.name);
        }
        for value in &self.args {
            my_size += ::protobuf::rt::string_size(2, &value);
        };
        my_size += ::protobuf::rt::unknown_fields_size(self.get_unknown_fields());
        self.cached_size.set(my_size);
        my_size
    }

    fn write_to_with_cached_sizes(&self, os: &mut ::protobuf::CodedOutputStream) -> ::protobuf::ProtobufResult<()> {
        if !self.name.is_empty() {
            os.write_string(1, &self.name)?;
        }
        for v in &self.args {
            os.write_string(2, &v)?;
        };
        os.write_unknown_fields(self.get_unknown_fields())?;
        ::std::result::Result::Ok(())
    }

    fn get_cached_size(&self) -> u32 {
        self.cached_size.get()
    }

    fn get_unknown_fields(&self) -> &::protobuf::UnknownFields {
        &self.unknown_fields
    }

    fn mut_unknown_fields(&mut self) -> &mut ::protobuf::UnknownFields {
        &mut self.unknown_fields
    }

    fn as_any(&self) -> &::std::any::Any {
        self as &::std::any::Any
    }
    fn as_any_mut(&mut self) -> &mut ::std::any::Any {
        self as &mut ::std::any::Any
    }
    fn into_any(self: Box<Self>) -> ::std::boxed::Box<::std::any::Any> {
        self
    }

    fn descriptor(&self) -> &'static ::protobuf::reflect::MessageDescriptor {
        ::protobuf::MessageStatic::descriptor_static(None::<Self>)
    }
}

impl ::protobuf::MessageStatic for ClientMsg_PlayerInput_Verb {
    fn new() -> ClientMsg_PlayerInput_Verb {
        ClientMsg_PlayerInput_Verb::new()
    }

    fn descriptor_static(_: ::std::option::Option<ClientMsg_PlayerInput_Verb>) -> &'static ::protobuf::reflect::MessageDescriptor {
        static mut descriptor: ::protobuf::lazy::Lazy<::protobuf::reflect::MessageDescriptor> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ::protobuf::reflect::MessageDescriptor,
        };
        unsafe {
            descriptor.get(|| {
                let mut fields = ::std::vec::Vec::new();
                fields.push(::protobuf::reflect::accessor::make_simple_field_accessor::<_, ::protobuf::types::ProtobufTypeString>(
                    "name",
                    ClientMsg_PlayerInput_Verb::get_name_for_reflect,
                    ClientMsg_PlayerInput_Verb::mut_name_for_reflect,
                ));
                fields.push(::protobuf::reflect::accessor::make_repeated_field_accessor::<_, ::protobuf::types::ProtobufTypeString>(
                    "args",
                    ClientMsg_PlayerInput_Verb::get_args_for_reflect,
                    ClientMsg_PlayerInput_Verb::mut_args_for_reflect,
                ));
                ::protobuf::reflect::MessageDescriptor::new::<ClientMsg_PlayerInput_Verb>(
                    "ClientMsg_PlayerInput_Verb",
                    fields,
                    file_descriptor_proto()
                )
            })
        }
    }
}

impl ::protobuf::Clear for ClientMsg_PlayerInput_Verb {
    fn clear(&mut self) {
        self.clear_name();
        self.clear_args();
        self.unknown_fields.clear();
    }
}

impl ::std::fmt::Debug for ClientMsg_PlayerInput_Verb {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::protobuf::text_format::fmt(self, f)
    }
}

impl ::protobuf::reflect::ProtobufValue for ClientMsg_PlayerInput_Verb {
    fn as_ref(&self) -> ::protobuf::reflect::ProtobufValueRef {
        ::protobuf::reflect::ProtobufValueRef::Message(self)
    }
}

#[derive(PartialEq,Clone,Default)]
pub struct ServerMsg {
    // message oneof groups
    msg: ::std::option::Option<ServerMsg_oneof_msg>,
    // special fields
    unknown_fields: ::protobuf::UnknownFields,
    cached_size: ::protobuf::CachedSize,
}

// see codegen.rs for the explanation why impl Sync explicitly
unsafe impl ::std::marker::Sync for ServerMsg {}

#[derive(Clone,PartialEq)]
pub enum ServerMsg_oneof_msg {
    welcome(ServerMsg_Welcome),
    world_state(ServerMsg_WorldState),
    update_obj(ServerMsg_UpdateObj),
    new_obj(ServerMsg_NewObj),
    del_obj(ServerMsg_DelObj),
    update_client(ServerMsg_UpdateClient),
}

impl ServerMsg {
    pub fn new() -> ServerMsg {
        ::std::default::Default::default()
    }

    pub fn default_instance() -> &'static ServerMsg {
        static mut instance: ::protobuf::lazy::Lazy<ServerMsg> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ServerMsg,
        };
        unsafe {
            instance.get(ServerMsg::new)
        }
    }

    // .ServerMsg.Welcome welcome = 1;

    pub fn clear_welcome(&mut self) {
        self.msg = ::std::option::Option::None;
    }

    pub fn has_welcome(&self) -> bool {
        match self.msg {
            ::std::option::Option::Some(ServerMsg_oneof_msg::welcome(..)) => true,
            _ => false,
        }
    }

    // Param is passed by value, moved
    pub fn set_welcome(&mut self, v: ServerMsg_Welcome) {
        self.msg = ::std::option::Option::Some(ServerMsg_oneof_msg::welcome(v))
    }

    // Mutable pointer to the field.
    pub fn mut_welcome(&mut self) -> &mut ServerMsg_Welcome {
        if let ::std::option::Option::Some(ServerMsg_oneof_msg::welcome(_)) = self.msg {
        } else {
            self.msg = ::std::option::Option::Some(ServerMsg_oneof_msg::welcome(ServerMsg_Welcome::new()));
        }
        match self.msg {
            ::std::option::Option::Some(ServerMsg_oneof_msg::welcome(ref mut v)) => v,
            _ => panic!(),
        }
    }

    // Take field
    pub fn take_welcome(&mut self) -> ServerMsg_Welcome {
        if self.has_welcome() {
            match self.msg.take() {
                ::std::option::Option::Some(ServerMsg_oneof_msg::welcome(v)) => v,
                _ => panic!(),
            }
        } else {
            ServerMsg_Welcome::new()
        }
    }

    pub fn get_welcome(&self) -> &ServerMsg_Welcome {
        match self.msg {
            ::std::option::Option::Some(ServerMsg_oneof_msg::welcome(ref v)) => v,
            _ => ServerMsg_Welcome::default_instance(),
        }
    }

    // .ServerMsg.WorldState world_state = 2;

    pub fn clear_world_state(&mut self) {
        self.msg = ::std::option::Option::None;
    }

    pub fn has_world_state(&self) -> bool {
        match self.msg {
            ::std::option::Option::Some(ServerMsg_oneof_msg::world_state(..)) => true,
            _ => false,
        }
    }

    // Param is passed by value, moved
    pub fn set_world_state(&mut self, v: ServerMsg_WorldState) {
        self.msg = ::std::option::Option::Some(ServerMsg_oneof_msg::world_state(v))
    }

    // Mutable pointer to the field.
    pub fn mut_world_state(&mut self) -> &mut ServerMsg_WorldState {
        if let ::std::option::Option::Some(ServerMsg_oneof_msg::world_state(_)) = self.msg {
        } else {
            self.msg = ::std::option::Option::Some(ServerMsg_oneof_msg::world_state(ServerMsg_WorldState::new()));
        }
        match self.msg {
            ::std::option::Option::Some(ServerMsg_oneof_msg::world_state(ref mut v)) => v,
            _ => panic!(),
        }
    }

    // Take field
    pub fn take_world_state(&mut self) -> ServerMsg_WorldState {
        if self.has_world_state() {
            match self.msg.take() {
                ::std::option::Option::Some(ServerMsg_oneof_msg::world_state(v)) => v,
                _ => panic!(),
            }
        } else {
            ServerMsg_WorldState::new()
        }
    }

    pub fn get_world_state(&self) -> &ServerMsg_WorldState {
        match self.msg {
            ::std::option::Option::Some(ServerMsg_oneof_msg::world_state(ref v)) => v,
            _ => ServerMsg_WorldState::default_instance(),
        }
    }

    // .ServerMsg.UpdateObj update_obj = 3;

    pub fn clear_update_obj(&mut self) {
        self.msg = ::std::option::Option::None;
    }

    pub fn has_update_obj(&self) -> bool {
        match self.msg {
            ::std::option::Option::Some(ServerMsg_oneof_msg::update_obj(..)) => true,
            _ => false,
        }
    }

    // Param is passed by value, moved
    pub fn set_update_obj(&mut self, v: ServerMsg_UpdateObj) {
        self.msg = ::std::option::Option::Some(ServerMsg_oneof_msg::update_obj(v))
    }

    // Mutable pointer to the field.
    pub fn mut_update_obj(&mut self) -> &mut ServerMsg_UpdateObj {
        if let ::std::option::Option::Some(ServerMsg_oneof_msg::update_obj(_)) = self.msg {
        } else {
            self.msg = ::std::option::Option::Some(ServerMsg_oneof_msg::update_obj(ServerMsg_UpdateObj::new()));
        }
        match self.msg {
            ::std::option::Option::Some(ServerMsg_oneof_msg::update_obj(ref mut v)) => v,
            _ => panic!(),
        }
    }

    // Take field
    pub fn take_update_obj(&mut self) -> ServerMsg_UpdateObj {
        if self.has_update_obj() {
            match self.msg.take() {
                ::std::option::Option::Some(ServerMsg_oneof_msg::update_obj(v)) => v,
                _ => panic!(),
            }
        } else {
            ServerMsg_UpdateObj::new()
        }
    }

    pub fn get_update_obj(&self) -> &ServerMsg_UpdateObj {
        match self.msg {
            ::std::option::Option::Some(ServerMsg_oneof_msg::update_obj(ref v)) => v,
            _ => ServerMsg_UpdateObj::default_instance(),
        }
    }

    // .ServerMsg.NewObj new_obj = 4;

    pub fn clear_new_obj(&mut self) {
        self.msg = ::std::option::Option::None;
    }

    pub fn has_new_obj(&self) -> bool {
        match self.msg {
            ::std::option::Option::Some(ServerMsg_oneof_msg::new_obj(..)) => true,
            _ => false,
        }
    }

    // Param is passed by value, moved
    pub fn set_new_obj(&mut self, v: ServerMsg_NewObj) {
        self.msg = ::std::option::Option::Some(ServerMsg_oneof_msg::new_obj(v))
    }

    // Mutable pointer to the field.
    pub fn mut_new_obj(&mut self) -> &mut ServerMsg_NewObj {
        if let ::std::option::Option::Some(ServerMsg_oneof_msg::new_obj(_)) = self.msg {
        } else {
            self.msg = ::std::option::Option::Some(ServerMsg_oneof_msg::new_obj(ServerMsg_NewObj::new()));
        }
        match self.msg {
            ::std::option::Option::Some(ServerMsg_oneof_msg::new_obj(ref mut v)) => v,
            _ => panic!(),
        }
    }

    // Take field
    pub fn take_new_obj(&mut self) -> ServerMsg_NewObj {
        if self.has_new_obj() {
            match self.msg.take() {
                ::std::option::Option::Some(ServerMsg_oneof_msg::new_obj(v)) => v,
                _ => panic!(),
            }
        } else {
            ServerMsg_NewObj::new()
        }
    }

    pub fn get_new_obj(&self) -> &ServerMsg_NewObj {
        match self.msg {
            ::std::option::Option::Some(ServerMsg_oneof_msg::new_obj(ref v)) => v,
            _ => ServerMsg_NewObj::default_instance(),
        }
    }

    // .ServerMsg.DelObj del_obj = 5;

    pub fn clear_del_obj(&mut self) {
        self.msg = ::std::option::Option::None;
    }

    pub fn has_del_obj(&self) -> bool {
        match self.msg {
            ::std::option::Option::Some(ServerMsg_oneof_msg::del_obj(..)) => true,
            _ => false,
        }
    }

    // Param is passed by value, moved
    pub fn set_del_obj(&mut self, v: ServerMsg_DelObj) {
        self.msg = ::std::option::Option::Some(ServerMsg_oneof_msg::del_obj(v))
    }

    // Mutable pointer to the field.
    pub fn mut_del_obj(&mut self) -> &mut ServerMsg_DelObj {
        if let ::std::option::Option::Some(ServerMsg_oneof_msg::del_obj(_)) = self.msg {
        } else {
            self.msg = ::std::option::Option::Some(ServerMsg_oneof_msg::del_obj(ServerMsg_DelObj::new()));
        }
        match self.msg {
            ::std::option::Option::Some(ServerMsg_oneof_msg::del_obj(ref mut v)) => v,
            _ => panic!(),
        }
    }

    // Take field
    pub fn take_del_obj(&mut self) -> ServerMsg_DelObj {
        if self.has_del_obj() {
            match self.msg.take() {
                ::std::option::Option::Some(ServerMsg_oneof_msg::del_obj(v)) => v,
                _ => panic!(),
            }
        } else {
            ServerMsg_DelObj::new()
        }
    }

    pub fn get_del_obj(&self) -> &ServerMsg_DelObj {
        match self.msg {
            ::std::option::Option::Some(ServerMsg_oneof_msg::del_obj(ref v)) => v,
            _ => ServerMsg_DelObj::default_instance(),
        }
    }

    // .ServerMsg.UpdateClient update_client = 6;

    pub fn clear_update_client(&mut self) {
        self.msg = ::std::option::Option::None;
    }

    pub fn has_update_client(&self) -> bool {
        match self.msg {
            ::std::option::Option::Some(ServerMsg_oneof_msg::update_client(..)) => true,
            _ => false,
        }
    }

    // Param is passed by value, moved
    pub fn set_update_client(&mut self, v: ServerMsg_UpdateClient) {
        self.msg = ::std::option::Option::Some(ServerMsg_oneof_msg::update_client(v))
    }

    // Mutable pointer to the field.
    pub fn mut_update_client(&mut self) -> &mut ServerMsg_UpdateClient {
        if let ::std::option::Option::Some(ServerMsg_oneof_msg::update_client(_)) = self.msg {
        } else {
            self.msg = ::std::option::Option::Some(ServerMsg_oneof_msg::update_client(ServerMsg_UpdateClient::new()));
        }
        match self.msg {
            ::std::option::Option::Some(ServerMsg_oneof_msg::update_client(ref mut v)) => v,
            _ => panic!(),
        }
    }

    // Take field
    pub fn take_update_client(&mut self) -> ServerMsg_UpdateClient {
        if self.has_update_client() {
            match self.msg.take() {
                ::std::option::Option::Some(ServerMsg_oneof_msg::update_client(v)) => v,
                _ => panic!(),
            }
        } else {
            ServerMsg_UpdateClient::new()
        }
    }

    pub fn get_update_client(&self) -> &ServerMsg_UpdateClient {
        match self.msg {
            ::std::option::Option::Some(ServerMsg_oneof_msg::update_client(ref v)) => v,
            _ => ServerMsg_UpdateClient::default_instance(),
        }
    }
}

impl ::protobuf::Message for ServerMsg {
    fn is_initialized(&self) -> bool {
        if let Some(ServerMsg_oneof_msg::welcome(ref v)) = self.msg {
            if !v.is_initialized() {
                return false;
            }
        }
        if let Some(ServerMsg_oneof_msg::world_state(ref v)) = self.msg {
            if !v.is_initialized() {
                return false;
            }
        }
        if let Some(ServerMsg_oneof_msg::update_obj(ref v)) = self.msg {
            if !v.is_initialized() {
                return false;
            }
        }
        if let Some(ServerMsg_oneof_msg::new_obj(ref v)) = self.msg {
            if !v.is_initialized() {
                return false;
            }
        }
        if let Some(ServerMsg_oneof_msg::del_obj(ref v)) = self.msg {
            if !v.is_initialized() {
                return false;
            }
        }
        if let Some(ServerMsg_oneof_msg::update_client(ref v)) = self.msg {
            if !v.is_initialized() {
                return false;
            }
        }
        true
    }

    fn merge_from(&mut self, is: &mut ::protobuf::CodedInputStream) -> ::protobuf::ProtobufResult<()> {
        while !is.eof()? {
            let (field_number, wire_type) = is.read_tag_unpack()?;
            match field_number {
                1 => {
                    if wire_type != ::protobuf::wire_format::WireTypeLengthDelimited {
                        return ::std::result::Result::Err(::protobuf::rt::unexpected_wire_type(wire_type));
                    }
                    self.msg = ::std::option::Option::Some(ServerMsg_oneof_msg::welcome(is.read_message()?));
                },
                2 => {
                    if wire_type != ::protobuf::wire_format::WireTypeLengthDelimited {
                        return ::std::result::Result::Err(::protobuf::rt::unexpected_wire_type(wire_type));
                    }
                    self.msg = ::std::option::Option::Some(ServerMsg_oneof_msg::world_state(is.read_message()?));
                },
                3 => {
                    if wire_type != ::protobuf::wire_format::WireTypeLengthDelimited {
                        return ::std::result::Result::Err(::protobuf::rt::unexpected_wire_type(wire_type));
                    }
                    self.msg = ::std::option::Option::Some(ServerMsg_oneof_msg::update_obj(is.read_message()?));
                },
                4 => {
                    if wire_type != ::protobuf::wire_format::WireTypeLengthDelimited {
                        return ::std::result::Result::Err(::protobuf::rt::unexpected_wire_type(wire_type));
                    }
                    self.msg = ::std::option::Option::Some(ServerMsg_oneof_msg::new_obj(is.read_message()?));
                },
                5 => {
                    if wire_type != ::protobuf::wire_format::WireTypeLengthDelimited {
                        return ::std::result::Result::Err(::protobuf::rt::unexpected_wire_type(wire_type));
                    }
                    self.msg = ::std::option::Option::Some(ServerMsg_oneof_msg::del_obj(is.read_message()?));
                },
                6 => {
                    if wire_type != ::protobuf::wire_format::WireTypeLengthDelimited {
                        return ::std::result::Result::Err(::protobuf::rt::unexpected_wire_type(wire_type));
                    }
                    self.msg = ::std::option::Option::Some(ServerMsg_oneof_msg::update_client(is.read_message()?));
                },
                _ => {
                    ::protobuf::rt::read_unknown_or_skip_group(field_number, wire_type, is, self.mut_unknown_fields())?;
                },
            };
        }
        ::std::result::Result::Ok(())
    }

    // Compute sizes of nested messages
    #[allow(unused_variables)]
    fn compute_size(&self) -> u32 {
        let mut my_size = 0;
        if let ::std::option::Option::Some(ref v) = self.msg {
            match v {
                &ServerMsg_oneof_msg::welcome(ref v) => {
                    let len = v.compute_size();
                    my_size += 1 + ::protobuf::rt::compute_raw_varint32_size(len) + len;
                },
                &ServerMsg_oneof_msg::world_state(ref v) => {
                    let len = v.compute_size();
                    my_size += 1 + ::protobuf::rt::compute_raw_varint32_size(len) + len;
                },
                &ServerMsg_oneof_msg::update_obj(ref v) => {
                    let len = v.compute_size();
                    my_size += 1 + ::protobuf::rt::compute_raw_varint32_size(len) + len;
                },
                &ServerMsg_oneof_msg::new_obj(ref v) => {
                    let len = v.compute_size();
                    my_size += 1 + ::protobuf::rt::compute_raw_varint32_size(len) + len;
                },
                &ServerMsg_oneof_msg::del_obj(ref v) => {
                    let len = v.compute_size();
                    my_size += 1 + ::protobuf::rt::compute_raw_varint32_size(len) + len;
                },
                &ServerMsg_oneof_msg::update_client(ref v) => {
                    let len = v.compute_size();
                    my_size += 1 + ::protobuf::rt::compute_raw_varint32_size(len) + len;
                },
            };
        }
        my_size += ::protobuf::rt::unknown_fields_size(self.get_unknown_fields());
        self.cached_size.set(my_size);
        my_size
    }

    fn write_to_with_cached_sizes(&self, os: &mut ::protobuf::CodedOutputStream) -> ::protobuf::ProtobufResult<()> {
        if let ::std::option::Option::Some(ref v) = self.msg {
            match v {
                &ServerMsg_oneof_msg::welcome(ref v) => {
                    os.write_tag(1, ::protobuf::wire_format::WireTypeLengthDelimited)?;
                    os.write_raw_varint32(v.get_cached_size())?;
                    v.write_to_with_cached_sizes(os)?;
                },
                &ServerMsg_oneof_msg::world_state(ref v) => {
                    os.write_tag(2, ::protobuf::wire_format::WireTypeLengthDelimited)?;
                    os.write_raw_varint32(v.get_cached_size())?;
                    v.write_to_with_cached_sizes(os)?;
                },
                &ServerMsg_oneof_msg::update_obj(ref v) => {
                    os.write_tag(3, ::protobuf::wire_format::WireTypeLengthDelimited)?;
                    os.write_raw_varint32(v.get_cached_size())?;
                    v.write_to_with_cached_sizes(os)?;
                },
                &ServerMsg_oneof_msg::new_obj(ref v) => {
                    os.write_tag(4, ::protobuf::wire_format::WireTypeLengthDelimited)?;
                    os.write_raw_varint32(v.get_cached_size())?;
                    v.write_to_with_cached_sizes(os)?;
                },
                &ServerMsg_oneof_msg::del_obj(ref v) => {
                    os.write_tag(5, ::protobuf::wire_format::WireTypeLengthDelimited)?;
                    os.write_raw_varint32(v.get_cached_size())?;
                    v.write_to_with_cached_sizes(os)?;
                },
                &ServerMsg_oneof_msg::update_client(ref v) => {
                    os.write_tag(6, ::protobuf::wire_format::WireTypeLengthDelimited)?;
                    os.write_raw_varint32(v.get_cached_size())?;
                    v.write_to_with_cached_sizes(os)?;
                },
            };
        }
        os.write_unknown_fields(self.get_unknown_fields())?;
        ::std::result::Result::Ok(())
    }

    fn get_cached_size(&self) -> u32 {
        self.cached_size.get()
    }

    fn get_unknown_fields(&self) -> &::protobuf::UnknownFields {
        &self.unknown_fields
    }

    fn mut_unknown_fields(&mut self) -> &mut ::protobuf::UnknownFields {
        &mut self.unknown_fields
    }

    fn as_any(&self) -> &::std::any::Any {
        self as &::std::any::Any
    }
    fn as_any_mut(&mut self) -> &mut ::std::any::Any {
        self as &mut ::std::any::Any
    }
    fn into_any(self: Box<Self>) -> ::std::boxed::Box<::std::any::Any> {
        self
    }

    fn descriptor(&self) -> &'static ::protobuf::reflect::MessageDescriptor {
        ::protobuf::MessageStatic::descriptor_static(None::<Self>)
    }
}

impl ::protobuf::MessageStatic for ServerMsg {
    fn new() -> ServerMsg {
        ServerMsg::new()
    }

    fn descriptor_static(_: ::std::option::Option<ServerMsg>) -> &'static ::protobuf::reflect::MessageDescriptor {
        static mut descriptor: ::protobuf::lazy::Lazy<::protobuf::reflect::MessageDescriptor> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ::protobuf::reflect::MessageDescriptor,
        };
        unsafe {
            descriptor.get(|| {
                let mut fields = ::std::vec::Vec::new();
                fields.push(::protobuf::reflect::accessor::make_singular_message_accessor::<_, ServerMsg_Welcome>(
                    "welcome",
                    ServerMsg::has_welcome,
                    ServerMsg::get_welcome,
                ));
                fields.push(::protobuf::reflect::accessor::make_singular_message_accessor::<_, ServerMsg_WorldState>(
                    "world_state",
                    ServerMsg::has_world_state,
                    ServerMsg::get_world_state,
                ));
                fields.push(::protobuf::reflect::accessor::make_singular_message_accessor::<_, ServerMsg_UpdateObj>(
                    "update_obj",
                    ServerMsg::has_update_obj,
                    ServerMsg::get_update_obj,
                ));
                fields.push(::protobuf::reflect::accessor::make_singular_message_accessor::<_, ServerMsg_NewObj>(
                    "new_obj",
                    ServerMsg::has_new_obj,
                    ServerMsg::get_new_obj,
                ));
                fields.push(::protobuf::reflect::accessor::make_singular_message_accessor::<_, ServerMsg_DelObj>(
                    "del_obj",
                    ServerMsg::has_del_obj,
                    ServerMsg::get_del_obj,
                ));
                fields.push(::protobuf::reflect::accessor::make_singular_message_accessor::<_, ServerMsg_UpdateClient>(
                    "update_client",
                    ServerMsg::has_update_client,
                    ServerMsg::get_update_client,
                ));
                ::protobuf::reflect::MessageDescriptor::new::<ServerMsg>(
                    "ServerMsg",
                    fields,
                    file_descriptor_proto()
                )
            })
        }
    }
}

impl ::protobuf::Clear for ServerMsg {
    fn clear(&mut self) {
        self.clear_welcome();
        self.clear_world_state();
        self.clear_update_obj();
        self.clear_new_obj();
        self.clear_del_obj();
        self.clear_update_client();
        self.unknown_fields.clear();
    }
}

impl ::std::fmt::Debug for ServerMsg {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::protobuf::text_format::fmt(self, f)
    }
}

impl ::protobuf::reflect::ProtobufValue for ServerMsg {
    fn as_ref(&self) -> ::protobuf::reflect::ProtobufValueRef {
        ::protobuf::reflect::ProtobufValueRef::Message(self)
    }
}

#[derive(PartialEq,Clone,Default)]
pub struct ServerMsg_Welcome {
    // message fields
    pub id: ::std::string::String,
    // special fields
    unknown_fields: ::protobuf::UnknownFields,
    cached_size: ::protobuf::CachedSize,
}

// see codegen.rs for the explanation why impl Sync explicitly
unsafe impl ::std::marker::Sync for ServerMsg_Welcome {}

impl ServerMsg_Welcome {
    pub fn new() -> ServerMsg_Welcome {
        ::std::default::Default::default()
    }

    pub fn default_instance() -> &'static ServerMsg_Welcome {
        static mut instance: ::protobuf::lazy::Lazy<ServerMsg_Welcome> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ServerMsg_Welcome,
        };
        unsafe {
            instance.get(ServerMsg_Welcome::new)
        }
    }

    // string id = 1;

    pub fn clear_id(&mut self) {
        self.id.clear();
    }

    // Param is passed by value, moved
    pub fn set_id(&mut self, v: ::std::string::String) {
        self.id = v;
    }

    // Mutable pointer to the field.
    // If field is not initialized, it is initialized with default value first.
    pub fn mut_id(&mut self) -> &mut ::std::string::String {
        &mut self.id
    }

    // Take field
    pub fn take_id(&mut self) -> ::std::string::String {
        ::std::mem::replace(&mut self.id, ::std::string::String::new())
    }

    pub fn get_id(&self) -> &str {
        &self.id
    }

    fn get_id_for_reflect(&self) -> &::std::string::String {
        &self.id
    }

    fn mut_id_for_reflect(&mut self) -> &mut ::std::string::String {
        &mut self.id
    }
}

impl ::protobuf::Message for ServerMsg_Welcome {
    fn is_initialized(&self) -> bool {
        true
    }

    fn merge_from(&mut self, is: &mut ::protobuf::CodedInputStream) -> ::protobuf::ProtobufResult<()> {
        while !is.eof()? {
            let (field_number, wire_type) = is.read_tag_unpack()?;
            match field_number {
                1 => {
                    ::protobuf::rt::read_singular_proto3_string_into(wire_type, is, &mut self.id)?;
                },
                _ => {
                    ::protobuf::rt::read_unknown_or_skip_group(field_number, wire_type, is, self.mut_unknown_fields())?;
                },
            };
        }
        ::std::result::Result::Ok(())
    }

    // Compute sizes of nested messages
    #[allow(unused_variables)]
    fn compute_size(&self) -> u32 {
        let mut my_size = 0;
        if !self.id.is_empty() {
            my_size += ::protobuf::rt::string_size(1, &self.id);
        }
        my_size += ::protobuf::rt::unknown_fields_size(self.get_unknown_fields());
        self.cached_size.set(my_size);
        my_size
    }

    fn write_to_with_cached_sizes(&self, os: &mut ::protobuf::CodedOutputStream) -> ::protobuf::ProtobufResult<()> {
        if !self.id.is_empty() {
            os.write_string(1, &self.id)?;
        }
        os.write_unknown_fields(self.get_unknown_fields())?;
        ::std::result::Result::Ok(())
    }

    fn get_cached_size(&self) -> u32 {
        self.cached_size.get()
    }

    fn get_unknown_fields(&self) -> &::protobuf::UnknownFields {
        &self.unknown_fields
    }

    fn mut_unknown_fields(&mut self) -> &mut ::protobuf::UnknownFields {
        &mut self.unknown_fields
    }

    fn as_any(&self) -> &::std::any::Any {
        self as &::std::any::Any
    }
    fn as_any_mut(&mut self) -> &mut ::std::any::Any {
        self as &mut ::std::any::Any
    }
    fn into_any(self: Box<Self>) -> ::std::boxed::Box<::std::any::Any> {
        self
    }

    fn descriptor(&self) -> &'static ::protobuf::reflect::MessageDescriptor {
        ::protobuf::MessageStatic::descriptor_static(None::<Self>)
    }
}

impl ::protobuf::MessageStatic for ServerMsg_Welcome {
    fn new() -> ServerMsg_Welcome {
        ServerMsg_Welcome::new()
    }

    fn descriptor_static(_: ::std::option::Option<ServerMsg_Welcome>) -> &'static ::protobuf::reflect::MessageDescriptor {
        static mut descriptor: ::protobuf::lazy::Lazy<::protobuf::reflect::MessageDescriptor> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ::protobuf::reflect::MessageDescriptor,
        };
        unsafe {
            descriptor.get(|| {
                let mut fields = ::std::vec::Vec::new();
                fields.push(::protobuf::reflect::accessor::make_simple_field_accessor::<_, ::protobuf::types::ProtobufTypeString>(
                    "id",
                    ServerMsg_Welcome::get_id_for_reflect,
                    ServerMsg_Welcome::mut_id_for_reflect,
                ));
                ::protobuf::reflect::MessageDescriptor::new::<ServerMsg_Welcome>(
                    "ServerMsg_Welcome",
                    fields,
                    file_descriptor_proto()
                )
            })
        }
    }
}

impl ::protobuf::Clear for ServerMsg_Welcome {
    fn clear(&mut self) {
        self.clear_id();
        self.unknown_fields.clear();
    }
}

impl ::std::fmt::Debug for ServerMsg_Welcome {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::protobuf::text_format::fmt(self, f)
    }
}

impl ::protobuf::reflect::ProtobufValue for ServerMsg_Welcome {
    fn as_ref(&self) -> ::protobuf::reflect::ProtobufValueRef {
        ::protobuf::reflect::ProtobufValueRef::Message(self)
    }
}

#[derive(PartialEq,Clone,Default)]
pub struct ServerMsg_WorldState {
    // message fields
    pub bounds: ::protobuf::SingularPtrField<Vector>,
    pub name: ::std::string::String,
    // special fields
    unknown_fields: ::protobuf::UnknownFields,
    cached_size: ::protobuf::CachedSize,
}

// see codegen.rs for the explanation why impl Sync explicitly
unsafe impl ::std::marker::Sync for ServerMsg_WorldState {}

impl ServerMsg_WorldState {
    pub fn new() -> ServerMsg_WorldState {
        ::std::default::Default::default()
    }

    pub fn default_instance() -> &'static ServerMsg_WorldState {
        static mut instance: ::protobuf::lazy::Lazy<ServerMsg_WorldState> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ServerMsg_WorldState,
        };
        unsafe {
            instance.get(ServerMsg_WorldState::new)
        }
    }

    // .Vector bounds = 1;

    pub fn clear_bounds(&mut self) {
        self.bounds.clear();
    }

    pub fn has_bounds(&self) -> bool {
        self.bounds.is_some()
    }

    // Param is passed by value, moved
    pub fn set_bounds(&mut self, v: Vector) {
        self.bounds = ::protobuf::SingularPtrField::some(v);
    }

    // Mutable pointer to the field.
    // If field is not initialized, it is initialized with default value first.
    pub fn mut_bounds(&mut self) -> &mut Vector {
        if self.bounds.is_none() {
            self.bounds.set_default();
        }
        self.bounds.as_mut().unwrap()
    }

    // Take field
    pub fn take_bounds(&mut self) -> Vector {
        self.bounds.take().unwrap_or_else(|| Vector::new())
    }

    pub fn get_bounds(&self) -> &Vector {
        self.bounds.as_ref().unwrap_or_else(|| Vector::default_instance())
    }

    fn get_bounds_for_reflect(&self) -> &::protobuf::SingularPtrField<Vector> {
        &self.bounds
    }

    fn mut_bounds_for_reflect(&mut self) -> &mut ::protobuf::SingularPtrField<Vector> {
        &mut self.bounds
    }

    // string name = 2;

    pub fn clear_name(&mut self) {
        self.name.clear();
    }

    // Param is passed by value, moved
    pub fn set_name(&mut self, v: ::std::string::String) {
        self.name = v;
    }

    // Mutable pointer to the field.
    // If field is not initialized, it is initialized with default value first.
    pub fn mut_name(&mut self) -> &mut ::std::string::String {
        &mut self.name
    }

    // Take field
    pub fn take_name(&mut self) -> ::std::string::String {
        ::std::mem::replace(&mut self.name, ::std::string::String::new())
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    fn get_name_for_reflect(&self) -> &::std::string::String {
        &self.name
    }

    fn mut_name_for_reflect(&mut self) -> &mut ::std::string::String {
        &mut self.name
    }
}

impl ::protobuf::Message for ServerMsg_WorldState {
    fn is_initialized(&self) -> bool {
        for v in &self.bounds {
            if !v.is_initialized() {
                return false;
            }
        };
        true
    }

    fn merge_from(&mut self, is: &mut ::protobuf::CodedInputStream) -> ::protobuf::ProtobufResult<()> {
        while !is.eof()? {
            let (field_number, wire_type) = is.read_tag_unpack()?;
            match field_number {
                1 => {
                    ::protobuf::rt::read_singular_message_into(wire_type, is, &mut self.bounds)?;
                },
                2 => {
                    ::protobuf::rt::read_singular_proto3_string_into(wire_type, is, &mut self.name)?;
                },
                _ => {
                    ::protobuf::rt::read_unknown_or_skip_group(field_number, wire_type, is, self.mut_unknown_fields())?;
                },
            };
        }
        ::std::result::Result::Ok(())
    }

    // Compute sizes of nested messages
    #[allow(unused_variables)]
    fn compute_size(&self) -> u32 {
        let mut my_size = 0;
        if let Some(ref v) = self.bounds.as_ref() {
            let len = v.compute_size();
            my_size += 1 + ::protobuf::rt::compute_raw_varint32_size(len) + len;
        }
        if !self.name.is_empty() {
            my_size += ::protobuf::rt::string_size(2, &self.name);
        }
        my_size += ::protobuf::rt::unknown_fields_size(self.get_unknown_fields());
        self.cached_size.set(my_size);
        my_size
    }

    fn write_to_with_cached_sizes(&self, os: &mut ::protobuf::CodedOutputStream) -> ::protobuf::ProtobufResult<()> {
        if let Some(ref v) = self.bounds.as_ref() {
            os.write_tag(1, ::protobuf::wire_format::WireTypeLengthDelimited)?;
            os.write_raw_varint32(v.get_cached_size())?;
            v.write_to_with_cached_sizes(os)?;
        }
        if !self.name.is_empty() {
            os.write_string(2, &self.name)?;
        }
        os.write_unknown_fields(self.get_unknown_fields())?;
        ::std::result::Result::Ok(())
    }

    fn get_cached_size(&self) -> u32 {
        self.cached_size.get()
    }

    fn get_unknown_fields(&self) -> &::protobuf::UnknownFields {
        &self.unknown_fields
    }

    fn mut_unknown_fields(&mut self) -> &mut ::protobuf::UnknownFields {
        &mut self.unknown_fields
    }

    fn as_any(&self) -> &::std::any::Any {
        self as &::std::any::Any
    }
    fn as_any_mut(&mut self) -> &mut ::std::any::Any {
        self as &mut ::std::any::Any
    }
    fn into_any(self: Box<Self>) -> ::std::boxed::Box<::std::any::Any> {
        self
    }

    fn descriptor(&self) -> &'static ::protobuf::reflect::MessageDescriptor {
        ::protobuf::MessageStatic::descriptor_static(None::<Self>)
    }
}

impl ::protobuf::MessageStatic for ServerMsg_WorldState {
    fn new() -> ServerMsg_WorldState {
        ServerMsg_WorldState::new()
    }

    fn descriptor_static(_: ::std::option::Option<ServerMsg_WorldState>) -> &'static ::protobuf::reflect::MessageDescriptor {
        static mut descriptor: ::protobuf::lazy::Lazy<::protobuf::reflect::MessageDescriptor> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ::protobuf::reflect::MessageDescriptor,
        };
        unsafe {
            descriptor.get(|| {
                let mut fields = ::std::vec::Vec::new();
                fields.push(::protobuf::reflect::accessor::make_singular_ptr_field_accessor::<_, ::protobuf::types::ProtobufTypeMessage<Vector>>(
                    "bounds",
                    ServerMsg_WorldState::get_bounds_for_reflect,
                    ServerMsg_WorldState::mut_bounds_for_reflect,
                ));
                fields.push(::protobuf::reflect::accessor::make_simple_field_accessor::<_, ::protobuf::types::ProtobufTypeString>(
                    "name",
                    ServerMsg_WorldState::get_name_for_reflect,
                    ServerMsg_WorldState::mut_name_for_reflect,
                ));
                ::protobuf::reflect::MessageDescriptor::new::<ServerMsg_WorldState>(
                    "ServerMsg_WorldState",
                    fields,
                    file_descriptor_proto()
                )
            })
        }
    }
}

impl ::protobuf::Clear for ServerMsg_WorldState {
    fn clear(&mut self) {
        self.clear_bounds();
        self.clear_name();
        self.unknown_fields.clear();
    }
}

impl ::std::fmt::Debug for ServerMsg_WorldState {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::protobuf::text_format::fmt(self, f)
    }
}

impl ::protobuf::reflect::ProtobufValue for ServerMsg_WorldState {
    fn as_ref(&self) -> ::protobuf::reflect::ProtobufValueRef {
        ::protobuf::reflect::ProtobufValueRef::Message(self)
    }
}

#[derive(PartialEq,Clone,Default)]
pub struct ServerMsg_ObjState {
    // message fields
    pub id: i64,
    pub name: ::std::string::String,
    // message oneof groups
    loc: ::std::option::Option<ServerMsg_ObjState_oneof_loc>,
    // special fields
    unknown_fields: ::protobuf::UnknownFields,
    cached_size: ::protobuf::CachedSize,
}

// see codegen.rs for the explanation why impl Sync explicitly
unsafe impl ::std::marker::Sync for ServerMsg_ObjState {}

#[derive(Clone,PartialEq)]
pub enum ServerMsg_ObjState_oneof_loc {
    inside(i32),
    position(Vector),
}

impl ServerMsg_ObjState {
    pub fn new() -> ServerMsg_ObjState {
        ::std::default::Default::default()
    }

    pub fn default_instance() -> &'static ServerMsg_ObjState {
        static mut instance: ::protobuf::lazy::Lazy<ServerMsg_ObjState> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ServerMsg_ObjState,
        };
        unsafe {
            instance.get(ServerMsg_ObjState::new)
        }
    }

    // int64 id = 1;

    pub fn clear_id(&mut self) {
        self.id = 0;
    }

    // Param is passed by value, moved
    pub fn set_id(&mut self, v: i64) {
        self.id = v;
    }

    pub fn get_id(&self) -> i64 {
        self.id
    }

    fn get_id_for_reflect(&self) -> &i64 {
        &self.id
    }

    fn mut_id_for_reflect(&mut self) -> &mut i64 {
        &mut self.id
    }

    // string name = 2;

    pub fn clear_name(&mut self) {
        self.name.clear();
    }

    // Param is passed by value, moved
    pub fn set_name(&mut self, v: ::std::string::String) {
        self.name = v;
    }

    // Mutable pointer to the field.
    // If field is not initialized, it is initialized with default value first.
    pub fn mut_name(&mut self) -> &mut ::std::string::String {
        &mut self.name
    }

    // Take field
    pub fn take_name(&mut self) -> ::std::string::String {
        ::std::mem::replace(&mut self.name, ::std::string::String::new())
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    fn get_name_for_reflect(&self) -> &::std::string::String {
        &self.name
    }

    fn mut_name_for_reflect(&mut self) -> &mut ::std::string::String {
        &mut self.name
    }

    // int32 inside = 4;

    pub fn clear_inside(&mut self) {
        self.loc = ::std::option::Option::None;
    }

    pub fn has_inside(&self) -> bool {
        match self.loc {
            ::std::option::Option::Some(ServerMsg_ObjState_oneof_loc::inside(..)) => true,
            _ => false,
        }
    }

    // Param is passed by value, moved
    pub fn set_inside(&mut self, v: i32) {
        self.loc = ::std::option::Option::Some(ServerMsg_ObjState_oneof_loc::inside(v))
    }

    pub fn get_inside(&self) -> i32 {
        match self.loc {
            ::std::option::Option::Some(ServerMsg_ObjState_oneof_loc::inside(v)) => v,
            _ => 0,
        }
    }

    // .Vector position = 5;

    pub fn clear_position(&mut self) {
        self.loc = ::std::option::Option::None;
    }

    pub fn has_position(&self) -> bool {
        match self.loc {
            ::std::option::Option::Some(ServerMsg_ObjState_oneof_loc::position(..)) => true,
            _ => false,
        }
    }

    // Param is passed by value, moved
    pub fn set_position(&mut self, v: Vector) {
        self.loc = ::std::option::Option::Some(ServerMsg_ObjState_oneof_loc::position(v))
    }

    // Mutable pointer to the field.
    pub fn mut_position(&mut self) -> &mut Vector {
        if let ::std::option::Option::Some(ServerMsg_ObjState_oneof_loc::position(_)) = self.loc {
        } else {
            self.loc = ::std::option::Option::Some(ServerMsg_ObjState_oneof_loc::position(Vector::new()));
        }
        match self.loc {
            ::std::option::Option::Some(ServerMsg_ObjState_oneof_loc::position(ref mut v)) => v,
            _ => panic!(),
        }
    }

    // Take field
    pub fn take_position(&mut self) -> Vector {
        if self.has_position() {
            match self.loc.take() {
                ::std::option::Option::Some(ServerMsg_ObjState_oneof_loc::position(v)) => v,
                _ => panic!(),
            }
        } else {
            Vector::new()
        }
    }

    pub fn get_position(&self) -> &Vector {
        match self.loc {
            ::std::option::Option::Some(ServerMsg_ObjState_oneof_loc::position(ref v)) => v,
            _ => Vector::default_instance(),
        }
    }
}

impl ::protobuf::Message for ServerMsg_ObjState {
    fn is_initialized(&self) -> bool {
        if let Some(ServerMsg_ObjState_oneof_loc::position(ref v)) = self.loc {
            if !v.is_initialized() {
                return false;
            }
        }
        true
    }

    fn merge_from(&mut self, is: &mut ::protobuf::CodedInputStream) -> ::protobuf::ProtobufResult<()> {
        while !is.eof()? {
            let (field_number, wire_type) = is.read_tag_unpack()?;
            match field_number {
                1 => {
                    if wire_type != ::protobuf::wire_format::WireTypeVarint {
                        return ::std::result::Result::Err(::protobuf::rt::unexpected_wire_type(wire_type));
                    }
                    let tmp = is.read_int64()?;
                    self.id = tmp;
                },
                2 => {
                    ::protobuf::rt::read_singular_proto3_string_into(wire_type, is, &mut self.name)?;
                },
                4 => {
                    if wire_type != ::protobuf::wire_format::WireTypeVarint {
                        return ::std::result::Result::Err(::protobuf::rt::unexpected_wire_type(wire_type));
                    }
                    self.loc = ::std::option::Option::Some(ServerMsg_ObjState_oneof_loc::inside(is.read_int32()?));
                },
                5 => {
                    if wire_type != ::protobuf::wire_format::WireTypeLengthDelimited {
                        return ::std::result::Result::Err(::protobuf::rt::unexpected_wire_type(wire_type));
                    }
                    self.loc = ::std::option::Option::Some(ServerMsg_ObjState_oneof_loc::position(is.read_message()?));
                },
                _ => {
                    ::protobuf::rt::read_unknown_or_skip_group(field_number, wire_type, is, self.mut_unknown_fields())?;
                },
            };
        }
        ::std::result::Result::Ok(())
    }

    // Compute sizes of nested messages
    #[allow(unused_variables)]
    fn compute_size(&self) -> u32 {
        let mut my_size = 0;
        if self.id != 0 {
            my_size += ::protobuf::rt::value_size(1, self.id, ::protobuf::wire_format::WireTypeVarint);
        }
        if !self.name.is_empty() {
            my_size += ::protobuf::rt::string_size(2, &self.name);
        }
        if let ::std::option::Option::Some(ref v) = self.loc {
            match v {
                &ServerMsg_ObjState_oneof_loc::inside(v) => {
                    my_size += ::protobuf::rt::value_size(4, v, ::protobuf::wire_format::WireTypeVarint);
                },
                &ServerMsg_ObjState_oneof_loc::position(ref v) => {
                    let len = v.compute_size();
                    my_size += 1 + ::protobuf::rt::compute_raw_varint32_size(len) + len;
                },
            };
        }
        my_size += ::protobuf::rt::unknown_fields_size(self.get_unknown_fields());
        self.cached_size.set(my_size);
        my_size
    }

    fn write_to_with_cached_sizes(&self, os: &mut ::protobuf::CodedOutputStream) -> ::protobuf::ProtobufResult<()> {
        if self.id != 0 {
            os.write_int64(1, self.id)?;
        }
        if !self.name.is_empty() {
            os.write_string(2, &self.name)?;
        }
        if let ::std::option::Option::Some(ref v) = self.loc {
            match v {
                &ServerMsg_ObjState_oneof_loc::inside(v) => {
                    os.write_int32(4, v)?;
                },
                &ServerMsg_ObjState_oneof_loc::position(ref v) => {
                    os.write_tag(5, ::protobuf::wire_format::WireTypeLengthDelimited)?;
                    os.write_raw_varint32(v.get_cached_size())?;
                    v.write_to_with_cached_sizes(os)?;
                },
            };
        }
        os.write_unknown_fields(self.get_unknown_fields())?;
        ::std::result::Result::Ok(())
    }

    fn get_cached_size(&self) -> u32 {
        self.cached_size.get()
    }

    fn get_unknown_fields(&self) -> &::protobuf::UnknownFields {
        &self.unknown_fields
    }

    fn mut_unknown_fields(&mut self) -> &mut ::protobuf::UnknownFields {
        &mut self.unknown_fields
    }

    fn as_any(&self) -> &::std::any::Any {
        self as &::std::any::Any
    }
    fn as_any_mut(&mut self) -> &mut ::std::any::Any {
        self as &mut ::std::any::Any
    }
    fn into_any(self: Box<Self>) -> ::std::boxed::Box<::std::any::Any> {
        self
    }

    fn descriptor(&self) -> &'static ::protobuf::reflect::MessageDescriptor {
        ::protobuf::MessageStatic::descriptor_static(None::<Self>)
    }
}

impl ::protobuf::MessageStatic for ServerMsg_ObjState {
    fn new() -> ServerMsg_ObjState {
        ServerMsg_ObjState::new()
    }

    fn descriptor_static(_: ::std::option::Option<ServerMsg_ObjState>) -> &'static ::protobuf::reflect::MessageDescriptor {
        static mut descriptor: ::protobuf::lazy::Lazy<::protobuf::reflect::MessageDescriptor> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ::protobuf::reflect::MessageDescriptor,
        };
        unsafe {
            descriptor.get(|| {
                let mut fields = ::std::vec::Vec::new();
                fields.push(::protobuf::reflect::accessor::make_simple_field_accessor::<_, ::protobuf::types::ProtobufTypeInt64>(
                    "id",
                    ServerMsg_ObjState::get_id_for_reflect,
                    ServerMsg_ObjState::mut_id_for_reflect,
                ));
                fields.push(::protobuf::reflect::accessor::make_simple_field_accessor::<_, ::protobuf::types::ProtobufTypeString>(
                    "name",
                    ServerMsg_ObjState::get_name_for_reflect,
                    ServerMsg_ObjState::mut_name_for_reflect,
                ));
                fields.push(::protobuf::reflect::accessor::make_singular_i32_accessor::<_>(
                    "inside",
                    ServerMsg_ObjState::has_inside,
                    ServerMsg_ObjState::get_inside,
                ));
                fields.push(::protobuf::reflect::accessor::make_singular_message_accessor::<_, Vector>(
                    "position",
                    ServerMsg_ObjState::has_position,
                    ServerMsg_ObjState::get_position,
                ));
                ::protobuf::reflect::MessageDescriptor::new::<ServerMsg_ObjState>(
                    "ServerMsg_ObjState",
                    fields,
                    file_descriptor_proto()
                )
            })
        }
    }
}

impl ::protobuf::Clear for ServerMsg_ObjState {
    fn clear(&mut self) {
        self.clear_id();
        self.clear_name();
        self.clear_inside();
        self.clear_position();
        self.unknown_fields.clear();
    }
}

impl ::std::fmt::Debug for ServerMsg_ObjState {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::protobuf::text_format::fmt(self, f)
    }
}

impl ::protobuf::reflect::ProtobufValue for ServerMsg_ObjState {
    fn as_ref(&self) -> ::protobuf::reflect::ProtobufValueRef {
        ::protobuf::reflect::ProtobufValueRef::Message(self)
    }
}

#[derive(PartialEq,Clone,Default)]
pub struct ServerMsg_UpdateObj {
    // message fields
    pub state: ::protobuf::SingularPtrField<ServerMsg_ObjState>,
    // special fields
    unknown_fields: ::protobuf::UnknownFields,
    cached_size: ::protobuf::CachedSize,
}

// see codegen.rs for the explanation why impl Sync explicitly
unsafe impl ::std::marker::Sync for ServerMsg_UpdateObj {}

impl ServerMsg_UpdateObj {
    pub fn new() -> ServerMsg_UpdateObj {
        ::std::default::Default::default()
    }

    pub fn default_instance() -> &'static ServerMsg_UpdateObj {
        static mut instance: ::protobuf::lazy::Lazy<ServerMsg_UpdateObj> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ServerMsg_UpdateObj,
        };
        unsafe {
            instance.get(ServerMsg_UpdateObj::new)
        }
    }

    // .ServerMsg.ObjState state = 1;

    pub fn clear_state(&mut self) {
        self.state.clear();
    }

    pub fn has_state(&self) -> bool {
        self.state.is_some()
    }

    // Param is passed by value, moved
    pub fn set_state(&mut self, v: ServerMsg_ObjState) {
        self.state = ::protobuf::SingularPtrField::some(v);
    }

    // Mutable pointer to the field.
    // If field is not initialized, it is initialized with default value first.
    pub fn mut_state(&mut self) -> &mut ServerMsg_ObjState {
        if self.state.is_none() {
            self.state.set_default();
        }
        self.state.as_mut().unwrap()
    }

    // Take field
    pub fn take_state(&mut self) -> ServerMsg_ObjState {
        self.state.take().unwrap_or_else(|| ServerMsg_ObjState::new())
    }

    pub fn get_state(&self) -> &ServerMsg_ObjState {
        self.state.as_ref().unwrap_or_else(|| ServerMsg_ObjState::default_instance())
    }

    fn get_state_for_reflect(&self) -> &::protobuf::SingularPtrField<ServerMsg_ObjState> {
        &self.state
    }

    fn mut_state_for_reflect(&mut self) -> &mut ::protobuf::SingularPtrField<ServerMsg_ObjState> {
        &mut self.state
    }
}

impl ::protobuf::Message for ServerMsg_UpdateObj {
    fn is_initialized(&self) -> bool {
        for v in &self.state {
            if !v.is_initialized() {
                return false;
            }
        };
        true
    }

    fn merge_from(&mut self, is: &mut ::protobuf::CodedInputStream) -> ::protobuf::ProtobufResult<()> {
        while !is.eof()? {
            let (field_number, wire_type) = is.read_tag_unpack()?;
            match field_number {
                1 => {
                    ::protobuf::rt::read_singular_message_into(wire_type, is, &mut self.state)?;
                },
                _ => {
                    ::protobuf::rt::read_unknown_or_skip_group(field_number, wire_type, is, self.mut_unknown_fields())?;
                },
            };
        }
        ::std::result::Result::Ok(())
    }

    // Compute sizes of nested messages
    #[allow(unused_variables)]
    fn compute_size(&self) -> u32 {
        let mut my_size = 0;
        if let Some(ref v) = self.state.as_ref() {
            let len = v.compute_size();
            my_size += 1 + ::protobuf::rt::compute_raw_varint32_size(len) + len;
        }
        my_size += ::protobuf::rt::unknown_fields_size(self.get_unknown_fields());
        self.cached_size.set(my_size);
        my_size
    }

    fn write_to_with_cached_sizes(&self, os: &mut ::protobuf::CodedOutputStream) -> ::protobuf::ProtobufResult<()> {
        if let Some(ref v) = self.state.as_ref() {
            os.write_tag(1, ::protobuf::wire_format::WireTypeLengthDelimited)?;
            os.write_raw_varint32(v.get_cached_size())?;
            v.write_to_with_cached_sizes(os)?;
        }
        os.write_unknown_fields(self.get_unknown_fields())?;
        ::std::result::Result::Ok(())
    }

    fn get_cached_size(&self) -> u32 {
        self.cached_size.get()
    }

    fn get_unknown_fields(&self) -> &::protobuf::UnknownFields {
        &self.unknown_fields
    }

    fn mut_unknown_fields(&mut self) -> &mut ::protobuf::UnknownFields {
        &mut self.unknown_fields
    }

    fn as_any(&self) -> &::std::any::Any {
        self as &::std::any::Any
    }
    fn as_any_mut(&mut self) -> &mut ::std::any::Any {
        self as &mut ::std::any::Any
    }
    fn into_any(self: Box<Self>) -> ::std::boxed::Box<::std::any::Any> {
        self
    }

    fn descriptor(&self) -> &'static ::protobuf::reflect::MessageDescriptor {
        ::protobuf::MessageStatic::descriptor_static(None::<Self>)
    }
}

impl ::protobuf::MessageStatic for ServerMsg_UpdateObj {
    fn new() -> ServerMsg_UpdateObj {
        ServerMsg_UpdateObj::new()
    }

    fn descriptor_static(_: ::std::option::Option<ServerMsg_UpdateObj>) -> &'static ::protobuf::reflect::MessageDescriptor {
        static mut descriptor: ::protobuf::lazy::Lazy<::protobuf::reflect::MessageDescriptor> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ::protobuf::reflect::MessageDescriptor,
        };
        unsafe {
            descriptor.get(|| {
                let mut fields = ::std::vec::Vec::new();
                fields.push(::protobuf::reflect::accessor::make_singular_ptr_field_accessor::<_, ::protobuf::types::ProtobufTypeMessage<ServerMsg_ObjState>>(
                    "state",
                    ServerMsg_UpdateObj::get_state_for_reflect,
                    ServerMsg_UpdateObj::mut_state_for_reflect,
                ));
                ::protobuf::reflect::MessageDescriptor::new::<ServerMsg_UpdateObj>(
                    "ServerMsg_UpdateObj",
                    fields,
                    file_descriptor_proto()
                )
            })
        }
    }
}

impl ::protobuf::Clear for ServerMsg_UpdateObj {
    fn clear(&mut self) {
        self.clear_state();
        self.unknown_fields.clear();
    }
}

impl ::std::fmt::Debug for ServerMsg_UpdateObj {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::protobuf::text_format::fmt(self, f)
    }
}

impl ::protobuf::reflect::ProtobufValue for ServerMsg_UpdateObj {
    fn as_ref(&self) -> ::protobuf::reflect::ProtobufValueRef {
        ::protobuf::reflect::ProtobufValueRef::Message(self)
    }
}

#[derive(PartialEq,Clone,Default)]
pub struct ServerMsg_NewObj {
    // message fields
    pub state: ::protobuf::SingularPtrField<ServerMsg_ObjState>,
    // special fields
    unknown_fields: ::protobuf::UnknownFields,
    cached_size: ::protobuf::CachedSize,
}

// see codegen.rs for the explanation why impl Sync explicitly
unsafe impl ::std::marker::Sync for ServerMsg_NewObj {}

impl ServerMsg_NewObj {
    pub fn new() -> ServerMsg_NewObj {
        ::std::default::Default::default()
    }

    pub fn default_instance() -> &'static ServerMsg_NewObj {
        static mut instance: ::protobuf::lazy::Lazy<ServerMsg_NewObj> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ServerMsg_NewObj,
        };
        unsafe {
            instance.get(ServerMsg_NewObj::new)
        }
    }

    // .ServerMsg.ObjState state = 1;

    pub fn clear_state(&mut self) {
        self.state.clear();
    }

    pub fn has_state(&self) -> bool {
        self.state.is_some()
    }

    // Param is passed by value, moved
    pub fn set_state(&mut self, v: ServerMsg_ObjState) {
        self.state = ::protobuf::SingularPtrField::some(v);
    }

    // Mutable pointer to the field.
    // If field is not initialized, it is initialized with default value first.
    pub fn mut_state(&mut self) -> &mut ServerMsg_ObjState {
        if self.state.is_none() {
            self.state.set_default();
        }
        self.state.as_mut().unwrap()
    }

    // Take field
    pub fn take_state(&mut self) -> ServerMsg_ObjState {
        self.state.take().unwrap_or_else(|| ServerMsg_ObjState::new())
    }

    pub fn get_state(&self) -> &ServerMsg_ObjState {
        self.state.as_ref().unwrap_or_else(|| ServerMsg_ObjState::default_instance())
    }

    fn get_state_for_reflect(&self) -> &::protobuf::SingularPtrField<ServerMsg_ObjState> {
        &self.state
    }

    fn mut_state_for_reflect(&mut self) -> &mut ::protobuf::SingularPtrField<ServerMsg_ObjState> {
        &mut self.state
    }
}

impl ::protobuf::Message for ServerMsg_NewObj {
    fn is_initialized(&self) -> bool {
        for v in &self.state {
            if !v.is_initialized() {
                return false;
            }
        };
        true
    }

    fn merge_from(&mut self, is: &mut ::protobuf::CodedInputStream) -> ::protobuf::ProtobufResult<()> {
        while !is.eof()? {
            let (field_number, wire_type) = is.read_tag_unpack()?;
            match field_number {
                1 => {
                    ::protobuf::rt::read_singular_message_into(wire_type, is, &mut self.state)?;
                },
                _ => {
                    ::protobuf::rt::read_unknown_or_skip_group(field_number, wire_type, is, self.mut_unknown_fields())?;
                },
            };
        }
        ::std::result::Result::Ok(())
    }

    // Compute sizes of nested messages
    #[allow(unused_variables)]
    fn compute_size(&self) -> u32 {
        let mut my_size = 0;
        if let Some(ref v) = self.state.as_ref() {
            let len = v.compute_size();
            my_size += 1 + ::protobuf::rt::compute_raw_varint32_size(len) + len;
        }
        my_size += ::protobuf::rt::unknown_fields_size(self.get_unknown_fields());
        self.cached_size.set(my_size);
        my_size
    }

    fn write_to_with_cached_sizes(&self, os: &mut ::protobuf::CodedOutputStream) -> ::protobuf::ProtobufResult<()> {
        if let Some(ref v) = self.state.as_ref() {
            os.write_tag(1, ::protobuf::wire_format::WireTypeLengthDelimited)?;
            os.write_raw_varint32(v.get_cached_size())?;
            v.write_to_with_cached_sizes(os)?;
        }
        os.write_unknown_fields(self.get_unknown_fields())?;
        ::std::result::Result::Ok(())
    }

    fn get_cached_size(&self) -> u32 {
        self.cached_size.get()
    }

    fn get_unknown_fields(&self) -> &::protobuf::UnknownFields {
        &self.unknown_fields
    }

    fn mut_unknown_fields(&mut self) -> &mut ::protobuf::UnknownFields {
        &mut self.unknown_fields
    }

    fn as_any(&self) -> &::std::any::Any {
        self as &::std::any::Any
    }
    fn as_any_mut(&mut self) -> &mut ::std::any::Any {
        self as &mut ::std::any::Any
    }
    fn into_any(self: Box<Self>) -> ::std::boxed::Box<::std::any::Any> {
        self
    }

    fn descriptor(&self) -> &'static ::protobuf::reflect::MessageDescriptor {
        ::protobuf::MessageStatic::descriptor_static(None::<Self>)
    }
}

impl ::protobuf::MessageStatic for ServerMsg_NewObj {
    fn new() -> ServerMsg_NewObj {
        ServerMsg_NewObj::new()
    }

    fn descriptor_static(_: ::std::option::Option<ServerMsg_NewObj>) -> &'static ::protobuf::reflect::MessageDescriptor {
        static mut descriptor: ::protobuf::lazy::Lazy<::protobuf::reflect::MessageDescriptor> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ::protobuf::reflect::MessageDescriptor,
        };
        unsafe {
            descriptor.get(|| {
                let mut fields = ::std::vec::Vec::new();
                fields.push(::protobuf::reflect::accessor::make_singular_ptr_field_accessor::<_, ::protobuf::types::ProtobufTypeMessage<ServerMsg_ObjState>>(
                    "state",
                    ServerMsg_NewObj::get_state_for_reflect,
                    ServerMsg_NewObj::mut_state_for_reflect,
                ));
                ::protobuf::reflect::MessageDescriptor::new::<ServerMsg_NewObj>(
                    "ServerMsg_NewObj",
                    fields,
                    file_descriptor_proto()
                )
            })
        }
    }
}

impl ::protobuf::Clear for ServerMsg_NewObj {
    fn clear(&mut self) {
        self.clear_state();
        self.unknown_fields.clear();
    }
}

impl ::std::fmt::Debug for ServerMsg_NewObj {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::protobuf::text_format::fmt(self, f)
    }
}

impl ::protobuf::reflect::ProtobufValue for ServerMsg_NewObj {
    fn as_ref(&self) -> ::protobuf::reflect::ProtobufValueRef {
        ::protobuf::reflect::ProtobufValueRef::Message(self)
    }
}

#[derive(PartialEq,Clone,Default)]
pub struct ServerMsg_DelObj {
    // message fields
    pub id: i64,
    // special fields
    unknown_fields: ::protobuf::UnknownFields,
    cached_size: ::protobuf::CachedSize,
}

// see codegen.rs for the explanation why impl Sync explicitly
unsafe impl ::std::marker::Sync for ServerMsg_DelObj {}

impl ServerMsg_DelObj {
    pub fn new() -> ServerMsg_DelObj {
        ::std::default::Default::default()
    }

    pub fn default_instance() -> &'static ServerMsg_DelObj {
        static mut instance: ::protobuf::lazy::Lazy<ServerMsg_DelObj> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ServerMsg_DelObj,
        };
        unsafe {
            instance.get(ServerMsg_DelObj::new)
        }
    }

    // int64 id = 1;

    pub fn clear_id(&mut self) {
        self.id = 0;
    }

    // Param is passed by value, moved
    pub fn set_id(&mut self, v: i64) {
        self.id = v;
    }

    pub fn get_id(&self) -> i64 {
        self.id
    }

    fn get_id_for_reflect(&self) -> &i64 {
        &self.id
    }

    fn mut_id_for_reflect(&mut self) -> &mut i64 {
        &mut self.id
    }
}

impl ::protobuf::Message for ServerMsg_DelObj {
    fn is_initialized(&self) -> bool {
        true
    }

    fn merge_from(&mut self, is: &mut ::protobuf::CodedInputStream) -> ::protobuf::ProtobufResult<()> {
        while !is.eof()? {
            let (field_number, wire_type) = is.read_tag_unpack()?;
            match field_number {
                1 => {
                    if wire_type != ::protobuf::wire_format::WireTypeVarint {
                        return ::std::result::Result::Err(::protobuf::rt::unexpected_wire_type(wire_type));
                    }
                    let tmp = is.read_int64()?;
                    self.id = tmp;
                },
                _ => {
                    ::protobuf::rt::read_unknown_or_skip_group(field_number, wire_type, is, self.mut_unknown_fields())?;
                },
            };
        }
        ::std::result::Result::Ok(())
    }

    // Compute sizes of nested messages
    #[allow(unused_variables)]
    fn compute_size(&self) -> u32 {
        let mut my_size = 0;
        if self.id != 0 {
            my_size += ::protobuf::rt::value_size(1, self.id, ::protobuf::wire_format::WireTypeVarint);
        }
        my_size += ::protobuf::rt::unknown_fields_size(self.get_unknown_fields());
        self.cached_size.set(my_size);
        my_size
    }

    fn write_to_with_cached_sizes(&self, os: &mut ::protobuf::CodedOutputStream) -> ::protobuf::ProtobufResult<()> {
        if self.id != 0 {
            os.write_int64(1, self.id)?;
        }
        os.write_unknown_fields(self.get_unknown_fields())?;
        ::std::result::Result::Ok(())
    }

    fn get_cached_size(&self) -> u32 {
        self.cached_size.get()
    }

    fn get_unknown_fields(&self) -> &::protobuf::UnknownFields {
        &self.unknown_fields
    }

    fn mut_unknown_fields(&mut self) -> &mut ::protobuf::UnknownFields {
        &mut self.unknown_fields
    }

    fn as_any(&self) -> &::std::any::Any {
        self as &::std::any::Any
    }
    fn as_any_mut(&mut self) -> &mut ::std::any::Any {
        self as &mut ::std::any::Any
    }
    fn into_any(self: Box<Self>) -> ::std::boxed::Box<::std::any::Any> {
        self
    }

    fn descriptor(&self) -> &'static ::protobuf::reflect::MessageDescriptor {
        ::protobuf::MessageStatic::descriptor_static(None::<Self>)
    }
}

impl ::protobuf::MessageStatic for ServerMsg_DelObj {
    fn new() -> ServerMsg_DelObj {
        ServerMsg_DelObj::new()
    }

    fn descriptor_static(_: ::std::option::Option<ServerMsg_DelObj>) -> &'static ::protobuf::reflect::MessageDescriptor {
        static mut descriptor: ::protobuf::lazy::Lazy<::protobuf::reflect::MessageDescriptor> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ::protobuf::reflect::MessageDescriptor,
        };
        unsafe {
            descriptor.get(|| {
                let mut fields = ::std::vec::Vec::new();
                fields.push(::protobuf::reflect::accessor::make_simple_field_accessor::<_, ::protobuf::types::ProtobufTypeInt64>(
                    "id",
                    ServerMsg_DelObj::get_id_for_reflect,
                    ServerMsg_DelObj::mut_id_for_reflect,
                ));
                ::protobuf::reflect::MessageDescriptor::new::<ServerMsg_DelObj>(
                    "ServerMsg_DelObj",
                    fields,
                    file_descriptor_proto()
                )
            })
        }
    }
}

impl ::protobuf::Clear for ServerMsg_DelObj {
    fn clear(&mut self) {
        self.clear_id();
        self.unknown_fields.clear();
    }
}

impl ::std::fmt::Debug for ServerMsg_DelObj {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::protobuf::text_format::fmt(self, f)
    }
}

impl ::protobuf::reflect::ProtobufValue for ServerMsg_DelObj {
    fn as_ref(&self) -> ::protobuf::reflect::ProtobufValueRef {
        ::protobuf::reflect::ProtobufValueRef::Message(self)
    }
}

#[derive(PartialEq,Clone,Default)]
pub struct ServerMsg_UpdateClient {
    // message fields
    pub ckey: ::std::string::String,
    pub container: i32,
    // special fields
    unknown_fields: ::protobuf::UnknownFields,
    cached_size: ::protobuf::CachedSize,
}

// see codegen.rs for the explanation why impl Sync explicitly
unsafe impl ::std::marker::Sync for ServerMsg_UpdateClient {}

impl ServerMsg_UpdateClient {
    pub fn new() -> ServerMsg_UpdateClient {
        ::std::default::Default::default()
    }

    pub fn default_instance() -> &'static ServerMsg_UpdateClient {
        static mut instance: ::protobuf::lazy::Lazy<ServerMsg_UpdateClient> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ServerMsg_UpdateClient,
        };
        unsafe {
            instance.get(ServerMsg_UpdateClient::new)
        }
    }

    // string ckey = 1;

    pub fn clear_ckey(&mut self) {
        self.ckey.clear();
    }

    // Param is passed by value, moved
    pub fn set_ckey(&mut self, v: ::std::string::String) {
        self.ckey = v;
    }

    // Mutable pointer to the field.
    // If field is not initialized, it is initialized with default value first.
    pub fn mut_ckey(&mut self) -> &mut ::std::string::String {
        &mut self.ckey
    }

    // Take field
    pub fn take_ckey(&mut self) -> ::std::string::String {
        ::std::mem::replace(&mut self.ckey, ::std::string::String::new())
    }

    pub fn get_ckey(&self) -> &str {
        &self.ckey
    }

    fn get_ckey_for_reflect(&self) -> &::std::string::String {
        &self.ckey
    }

    fn mut_ckey_for_reflect(&mut self) -> &mut ::std::string::String {
        &mut self.ckey
    }

    // int32 container = 2;

    pub fn clear_container(&mut self) {
        self.container = 0;
    }

    // Param is passed by value, moved
    pub fn set_container(&mut self, v: i32) {
        self.container = v;
    }

    pub fn get_container(&self) -> i32 {
        self.container
    }

    fn get_container_for_reflect(&self) -> &i32 {
        &self.container
    }

    fn mut_container_for_reflect(&mut self) -> &mut i32 {
        &mut self.container
    }
}

impl ::protobuf::Message for ServerMsg_UpdateClient {
    fn is_initialized(&self) -> bool {
        true
    }

    fn merge_from(&mut self, is: &mut ::protobuf::CodedInputStream) -> ::protobuf::ProtobufResult<()> {
        while !is.eof()? {
            let (field_number, wire_type) = is.read_tag_unpack()?;
            match field_number {
                1 => {
                    ::protobuf::rt::read_singular_proto3_string_into(wire_type, is, &mut self.ckey)?;
                },
                2 => {
                    if wire_type != ::protobuf::wire_format::WireTypeVarint {
                        return ::std::result::Result::Err(::protobuf::rt::unexpected_wire_type(wire_type));
                    }
                    let tmp = is.read_int32()?;
                    self.container = tmp;
                },
                _ => {
                    ::protobuf::rt::read_unknown_or_skip_group(field_number, wire_type, is, self.mut_unknown_fields())?;
                },
            };
        }
        ::std::result::Result::Ok(())
    }

    // Compute sizes of nested messages
    #[allow(unused_variables)]
    fn compute_size(&self) -> u32 {
        let mut my_size = 0;
        if !self.ckey.is_empty() {
            my_size += ::protobuf::rt::string_size(1, &self.ckey);
        }
        if self.container != 0 {
            my_size += ::protobuf::rt::value_size(2, self.container, ::protobuf::wire_format::WireTypeVarint);
        }
        my_size += ::protobuf::rt::unknown_fields_size(self.get_unknown_fields());
        self.cached_size.set(my_size);
        my_size
    }

    fn write_to_with_cached_sizes(&self, os: &mut ::protobuf::CodedOutputStream) -> ::protobuf::ProtobufResult<()> {
        if !self.ckey.is_empty() {
            os.write_string(1, &self.ckey)?;
        }
        if self.container != 0 {
            os.write_int32(2, self.container)?;
        }
        os.write_unknown_fields(self.get_unknown_fields())?;
        ::std::result::Result::Ok(())
    }

    fn get_cached_size(&self) -> u32 {
        self.cached_size.get()
    }

    fn get_unknown_fields(&self) -> &::protobuf::UnknownFields {
        &self.unknown_fields
    }

    fn mut_unknown_fields(&mut self) -> &mut ::protobuf::UnknownFields {
        &mut self.unknown_fields
    }

    fn as_any(&self) -> &::std::any::Any {
        self as &::std::any::Any
    }
    fn as_any_mut(&mut self) -> &mut ::std::any::Any {
        self as &mut ::std::any::Any
    }
    fn into_any(self: Box<Self>) -> ::std::boxed::Box<::std::any::Any> {
        self
    }

    fn descriptor(&self) -> &'static ::protobuf::reflect::MessageDescriptor {
        ::protobuf::MessageStatic::descriptor_static(None::<Self>)
    }
}

impl ::protobuf::MessageStatic for ServerMsg_UpdateClient {
    fn new() -> ServerMsg_UpdateClient {
        ServerMsg_UpdateClient::new()
    }

    fn descriptor_static(_: ::std::option::Option<ServerMsg_UpdateClient>) -> &'static ::protobuf::reflect::MessageDescriptor {
        static mut descriptor: ::protobuf::lazy::Lazy<::protobuf::reflect::MessageDescriptor> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ::protobuf::reflect::MessageDescriptor,
        };
        unsafe {
            descriptor.get(|| {
                let mut fields = ::std::vec::Vec::new();
                fields.push(::protobuf::reflect::accessor::make_simple_field_accessor::<_, ::protobuf::types::ProtobufTypeString>(
                    "ckey",
                    ServerMsg_UpdateClient::get_ckey_for_reflect,
                    ServerMsg_UpdateClient::mut_ckey_for_reflect,
                ));
                fields.push(::protobuf::reflect::accessor::make_simple_field_accessor::<_, ::protobuf::types::ProtobufTypeInt32>(
                    "container",
                    ServerMsg_UpdateClient::get_container_for_reflect,
                    ServerMsg_UpdateClient::mut_container_for_reflect,
                ));
                ::protobuf::reflect::MessageDescriptor::new::<ServerMsg_UpdateClient>(
                    "ServerMsg_UpdateClient",
                    fields,
                    file_descriptor_proto()
                )
            })
        }
    }
}

impl ::protobuf::Clear for ServerMsg_UpdateClient {
    fn clear(&mut self) {
        self.clear_ckey();
        self.clear_container();
        self.unknown_fields.clear();
    }
}

impl ::std::fmt::Debug for ServerMsg_UpdateClient {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::protobuf::text_format::fmt(self, f)
    }
}

impl ::protobuf::reflect::ProtobufValue for ServerMsg_UpdateClient {
    fn as_ref(&self) -> ::protobuf::reflect::ProtobufValueRef {
        ::protobuf::reflect::ProtobufValueRef::Message(self)
    }
}

#[derive(PartialEq,Clone,Default)]
pub struct Vector {
    // message fields
    pub x: i32,
    pub y: i32,
    pub z: i32,
    // special fields
    unknown_fields: ::protobuf::UnknownFields,
    cached_size: ::protobuf::CachedSize,
}

// see codegen.rs for the explanation why impl Sync explicitly
unsafe impl ::std::marker::Sync for Vector {}

impl Vector {
    pub fn new() -> Vector {
        ::std::default::Default::default()
    }

    pub fn default_instance() -> &'static Vector {
        static mut instance: ::protobuf::lazy::Lazy<Vector> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const Vector,
        };
        unsafe {
            instance.get(Vector::new)
        }
    }

    // int32 x = 1;

    pub fn clear_x(&mut self) {
        self.x = 0;
    }

    // Param is passed by value, moved
    pub fn set_x(&mut self, v: i32) {
        self.x = v;
    }

    pub fn get_x(&self) -> i32 {
        self.x
    }

    fn get_x_for_reflect(&self) -> &i32 {
        &self.x
    }

    fn mut_x_for_reflect(&mut self) -> &mut i32 {
        &mut self.x
    }

    // int32 y = 2;

    pub fn clear_y(&mut self) {
        self.y = 0;
    }

    // Param is passed by value, moved
    pub fn set_y(&mut self, v: i32) {
        self.y = v;
    }

    pub fn get_y(&self) -> i32 {
        self.y
    }

    fn get_y_for_reflect(&self) -> &i32 {
        &self.y
    }

    fn mut_y_for_reflect(&mut self) -> &mut i32 {
        &mut self.y
    }

    // int32 z = 3;

    pub fn clear_z(&mut self) {
        self.z = 0;
    }

    // Param is passed by value, moved
    pub fn set_z(&mut self, v: i32) {
        self.z = v;
    }

    pub fn get_z(&self) -> i32 {
        self.z
    }

    fn get_z_for_reflect(&self) -> &i32 {
        &self.z
    }

    fn mut_z_for_reflect(&mut self) -> &mut i32 {
        &mut self.z
    }
}

impl ::protobuf::Message for Vector {
    fn is_initialized(&self) -> bool {
        true
    }

    fn merge_from(&mut self, is: &mut ::protobuf::CodedInputStream) -> ::protobuf::ProtobufResult<()> {
        while !is.eof()? {
            let (field_number, wire_type) = is.read_tag_unpack()?;
            match field_number {
                1 => {
                    if wire_type != ::protobuf::wire_format::WireTypeVarint {
                        return ::std::result::Result::Err(::protobuf::rt::unexpected_wire_type(wire_type));
                    }
                    let tmp = is.read_int32()?;
                    self.x = tmp;
                },
                2 => {
                    if wire_type != ::protobuf::wire_format::WireTypeVarint {
                        return ::std::result::Result::Err(::protobuf::rt::unexpected_wire_type(wire_type));
                    }
                    let tmp = is.read_int32()?;
                    self.y = tmp;
                },
                3 => {
                    if wire_type != ::protobuf::wire_format::WireTypeVarint {
                        return ::std::result::Result::Err(::protobuf::rt::unexpected_wire_type(wire_type));
                    }
                    let tmp = is.read_int32()?;
                    self.z = tmp;
                },
                _ => {
                    ::protobuf::rt::read_unknown_or_skip_group(field_number, wire_type, is, self.mut_unknown_fields())?;
                },
            };
        }
        ::std::result::Result::Ok(())
    }

    // Compute sizes of nested messages
    #[allow(unused_variables)]
    fn compute_size(&self) -> u32 {
        let mut my_size = 0;
        if self.x != 0 {
            my_size += ::protobuf::rt::value_size(1, self.x, ::protobuf::wire_format::WireTypeVarint);
        }
        if self.y != 0 {
            my_size += ::protobuf::rt::value_size(2, self.y, ::protobuf::wire_format::WireTypeVarint);
        }
        if self.z != 0 {
            my_size += ::protobuf::rt::value_size(3, self.z, ::protobuf::wire_format::WireTypeVarint);
        }
        my_size += ::protobuf::rt::unknown_fields_size(self.get_unknown_fields());
        self.cached_size.set(my_size);
        my_size
    }

    fn write_to_with_cached_sizes(&self, os: &mut ::protobuf::CodedOutputStream) -> ::protobuf::ProtobufResult<()> {
        if self.x != 0 {
            os.write_int32(1, self.x)?;
        }
        if self.y != 0 {
            os.write_int32(2, self.y)?;
        }
        if self.z != 0 {
            os.write_int32(3, self.z)?;
        }
        os.write_unknown_fields(self.get_unknown_fields())?;
        ::std::result::Result::Ok(())
    }

    fn get_cached_size(&self) -> u32 {
        self.cached_size.get()
    }

    fn get_unknown_fields(&self) -> &::protobuf::UnknownFields {
        &self.unknown_fields
    }

    fn mut_unknown_fields(&mut self) -> &mut ::protobuf::UnknownFields {
        &mut self.unknown_fields
    }

    fn as_any(&self) -> &::std::any::Any {
        self as &::std::any::Any
    }
    fn as_any_mut(&mut self) -> &mut ::std::any::Any {
        self as &mut ::std::any::Any
    }
    fn into_any(self: Box<Self>) -> ::std::boxed::Box<::std::any::Any> {
        self
    }

    fn descriptor(&self) -> &'static ::protobuf::reflect::MessageDescriptor {
        ::protobuf::MessageStatic::descriptor_static(None::<Self>)
    }
}

impl ::protobuf::MessageStatic for Vector {
    fn new() -> Vector {
        Vector::new()
    }

    fn descriptor_static(_: ::std::option::Option<Vector>) -> &'static ::protobuf::reflect::MessageDescriptor {
        static mut descriptor: ::protobuf::lazy::Lazy<::protobuf::reflect::MessageDescriptor> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ::protobuf::reflect::MessageDescriptor,
        };
        unsafe {
            descriptor.get(|| {
                let mut fields = ::std::vec::Vec::new();
                fields.push(::protobuf::reflect::accessor::make_simple_field_accessor::<_, ::protobuf::types::ProtobufTypeInt32>(
                    "x",
                    Vector::get_x_for_reflect,
                    Vector::mut_x_for_reflect,
                ));
                fields.push(::protobuf::reflect::accessor::make_simple_field_accessor::<_, ::protobuf::types::ProtobufTypeInt32>(
                    "y",
                    Vector::get_y_for_reflect,
                    Vector::mut_y_for_reflect,
                ));
                fields.push(::protobuf::reflect::accessor::make_simple_field_accessor::<_, ::protobuf::types::ProtobufTypeInt32>(
                    "z",
                    Vector::get_z_for_reflect,
                    Vector::mut_z_for_reflect,
                ));
                ::protobuf::reflect::MessageDescriptor::new::<Vector>(
                    "Vector",
                    fields,
                    file_descriptor_proto()
                )
            })
        }
    }
}

impl ::protobuf::Clear for Vector {
    fn clear(&mut self) {
        self.clear_x();
        self.clear_y();
        self.clear_z();
        self.unknown_fields.clear();
    }
}

impl ::std::fmt::Debug for Vector {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::protobuf::text_format::fmt(self, f)
    }
}

impl ::protobuf::reflect::ProtobufValue for Vector {
    fn as_ref(&self) -> ::protobuf::reflect::ProtobufValueRef {
        ::protobuf::reflect::ProtobufValueRef::Message(self)
    }
}

#[derive(Clone,PartialEq,Eq,Debug,Hash)]
pub enum Direction {
    UNKNOWN = 0,
    NORTH = 1,
    SOUTH = 2,
    EAST = 4,
    WEST = 8,
    NORTHEAST = 5,
    NORTHWEST = 9,
    SOUTHEAST = 6,
    SOUTHWEST = 10,
    UP = 16,
    DOWN = 32,
}

impl ::protobuf::ProtobufEnum for Direction {
    fn value(&self) -> i32 {
        *self as i32
    }

    fn from_i32(value: i32) -> ::std::option::Option<Direction> {
        match value {
            0 => ::std::option::Option::Some(Direction::UNKNOWN),
            1 => ::std::option::Option::Some(Direction::NORTH),
            2 => ::std::option::Option::Some(Direction::SOUTH),
            4 => ::std::option::Option::Some(Direction::EAST),
            8 => ::std::option::Option::Some(Direction::WEST),
            5 => ::std::option::Option::Some(Direction::NORTHEAST),
            9 => ::std::option::Option::Some(Direction::NORTHWEST),
            6 => ::std::option::Option::Some(Direction::SOUTHEAST),
            10 => ::std::option::Option::Some(Direction::SOUTHWEST),
            16 => ::std::option::Option::Some(Direction::UP),
            32 => ::std::option::Option::Some(Direction::DOWN),
            _ => ::std::option::Option::None
        }
    }

    fn values() -> &'static [Self] {
        static values: &'static [Direction] = &[
            Direction::UNKNOWN,
            Direction::NORTH,
            Direction::SOUTH,
            Direction::EAST,
            Direction::WEST,
            Direction::NORTHEAST,
            Direction::NORTHWEST,
            Direction::SOUTHEAST,
            Direction::SOUTHWEST,
            Direction::UP,
            Direction::DOWN,
        ];
        values
    }

    fn enum_descriptor_static(_: ::std::option::Option<Direction>) -> &'static ::protobuf::reflect::EnumDescriptor {
        static mut descriptor: ::protobuf::lazy::Lazy<::protobuf::reflect::EnumDescriptor> = ::protobuf::lazy::Lazy {
            lock: ::protobuf::lazy::ONCE_INIT,
            ptr: 0 as *const ::protobuf::reflect::EnumDescriptor,
        };
        unsafe {
            descriptor.get(|| {
                ::protobuf::reflect::EnumDescriptor::new("Direction", file_descriptor_proto())
            })
        }
    }
}

impl ::std::marker::Copy for Direction {
}

impl ::std::default::Default for Direction {
    fn default() -> Self {
        Direction::UNKNOWN
    }
}

impl ::protobuf::reflect::ProtobufValue for Direction {
    fn as_ref(&self) -> ::protobuf::reflect::ProtobufValueRef {
        ::protobuf::reflect::ProtobufValueRef::Enum(self.descriptor())
    }
}

static file_descriptor_proto_data: &'static [u8] = b"\
    \n\trpc.proto\"\xc6\x05\n\tClientMsg\x12(\n\x05setup\x18\x01\x20\x01(\
    \x0b2\x10.ClientMsg.SetupH\0R\x05setup\x12A\n\x0eplayer_connect\x18\x02\
    \x20\x01(\x0b2\x18.ClientMsg.PlayerConnectH\0R\rplayerConnect\x12J\n\x11\
    player_disconnect\x18\x03\x20\x01(\x0b2\x1b.ClientMsg.PlayerDisconnectH\
    \0R\x10playerDisconnect\x12;\n\x0cplayer_input\x18\x04\x20\x01(\x0b2\x16\
    .ClientMsg.PlayerInputH\0R\x0bplayerInput\x1a\x1b\n\x05Setup\x12\x12\n\
    \x04host\x18\x01\x20\x01(\tR\x04host\x1aE\n\rPlayerConnect\x12\x12\n\x04\
    ckey\x18\x01\x20\x01(\tR\x04ckey\x12\x0e\n\x02ip\x18\x02\x20\x01(\tR\x02\
    ip\x12\x10\n\x03cid\x18\x03\x20\x01(\tR\x03cid\x1a&\n\x10PlayerDisconnec\
    t\x12\x12\n\x04ckey\x18\x01\x20\x01(\tR\x04ckey\x1a\xaf\x02\n\x0bPlayerI\
    nput\x12\x12\n\x04ckey\x18\x01\x20\x01(\tR\x04ckey\x12(\n\x08movement\
    \x18\x02\x20\x01(\x0e2\n.DirectionH\0R\x08movement\x124\n\x05click\x18\
    \x03\x20\x01(\x0b2\x1c.ClientMsg.PlayerInput.ClickH\0R\x05click\x121\n\
    \x04verb\x18\x04\x20\x01(\x0b2\x1b.ClientMsg.PlayerInput.VerbH\0R\x04ver\
    b\x1a@\n\x05Click\x12\x16\n\x06target\x18\x01\x20\x01(\x03R\x06target\
    \x12\x1f\n\x06offset\x18\x02\x20\x01(\x0b2\x07.VectorR\x06offset\x1a.\n\
    \x04Verb\x12\x12\n\x04name\x18\x01\x20\x01(\tR\x04name\x12\x12\n\x04args\
    \x18\x02\x20\x03(\tR\x04argsB\x07\n\x05eventB\x05\n\x03msg\"\xee\x05\n\t\
    ServerMsg\x12.\n\x07welcome\x18\x01\x20\x01(\x0b2\x12.ServerMsg.WelcomeH\
    \0R\x07welcome\x128\n\x0bworld_state\x18\x02\x20\x01(\x0b2\x15.ServerMsg\
    .WorldStateH\0R\nworldState\x125\n\nupdate_obj\x18\x03\x20\x01(\x0b2\x14\
    .ServerMsg.UpdateObjH\0R\tupdateObj\x12,\n\x07new_obj\x18\x04\x20\x01(\
    \x0b2\x11.ServerMsg.NewObjH\0R\x06newObj\x12,\n\x07del_obj\x18\x05\x20\
    \x01(\x0b2\x11.ServerMsg.DelObjH\0R\x06delObj\x12>\n\rupdate_client\x18\
    \x06\x20\x01(\x0b2\x17.ServerMsg.UpdateClientH\0R\x0cupdateClient\x1a\
    \x19\n\x07Welcome\x12\x0e\n\x02id\x18\x01\x20\x01(\tR\x02id\x1aA\n\nWorl\
    dState\x12\x1f\n\x06bounds\x18\x01\x20\x01(\x0b2\x07.VectorR\x06bounds\
    \x12\x12\n\x04name\x18\x02\x20\x01(\tR\x04name\x1av\n\x08ObjState\x12\
    \x0e\n\x02id\x18\x01\x20\x01(\x03R\x02id\x12\x12\n\x04name\x18\x02\x20\
    \x01(\tR\x04name\x12\x18\n\x06inside\x18\x04\x20\x01(\x05H\0R\x06inside\
    \x12%\n\x08position\x18\x05\x20\x01(\x0b2\x07.VectorH\0R\x08positionB\
    \x05\n\x03loc\x1a6\n\tUpdateObj\x12)\n\x05state\x18\x01\x20\x01(\x0b2\
    \x13.ServerMsg.ObjStateR\x05state\x1a3\n\x06NewObj\x12)\n\x05state\x18\
    \x01\x20\x01(\x0b2\x13.ServerMsg.ObjStateR\x05state\x1a\x18\n\x06DelObj\
    \x12\x0e\n\x02id\x18\x01\x20\x01(\x03R\x02id\x1a@\n\x0cUpdateClient\x12\
    \x12\n\x04ckey\x18\x01\x20\x01(\tR\x04ckey\x12\x1c\n\tcontainer\x18\x02\
    \x20\x01(\x05R\tcontainerB\x05\n\x03msg\"2\n\x06Vector\x12\x0c\n\x01x\
    \x18\x01\x20\x01(\x05R\x01x\x12\x0c\n\x01y\x18\x02\x20\x01(\x05R\x01y\
    \x12\x0c\n\x01z\x18\x03\x20\x01(\x05R\x01z*\x90\x01\n\tDirection\x12\x0b\
    \n\x07UNKNOWN\x10\0\x12\t\n\x05NORTH\x10\x01\x12\t\n\x05SOUTH\x10\x02\
    \x12\x08\n\x04EAST\x10\x04\x12\x08\n\x04WEST\x10\x08\x12\r\n\tNORTHEAST\
    \x10\x05\x12\r\n\tNORTHWEST\x10\t\x12\r\n\tSOUTHEAST\x10\x06\x12\r\n\tSO\
    UTHWEST\x10\n\x12\x06\n\x02UP\x10\x10\x12\x08\n\x04DOWN\x10\x202+\n\x04D\
    iby\x12#\n\x05Serve\x12\n.ClientMsg\x1a\n.ServerMsg(\x010\x01b\x06proto3\
";

static mut file_descriptor_proto_lazy: ::protobuf::lazy::Lazy<::protobuf::descriptor::FileDescriptorProto> = ::protobuf::lazy::Lazy {
    lock: ::protobuf::lazy::ONCE_INIT,
    ptr: 0 as *const ::protobuf::descriptor::FileDescriptorProto,
};

fn parse_descriptor_proto() -> ::protobuf::descriptor::FileDescriptorProto {
    ::protobuf::parse_from_bytes(file_descriptor_proto_data).unwrap()
}

pub fn file_descriptor_proto() -> &'static ::protobuf::descriptor::FileDescriptorProto {
    unsafe {
        file_descriptor_proto_lazy.get(|| {
            parse_descriptor_proto()
        })
    }
}
