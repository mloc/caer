// Copyright 2016 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#[macro_use] extern crate serde_derive;
extern crate serde;

use std::marker::PhantomData;

use std::fmt::{self, Debug, Formatter};
use std::vec::IntoIter;
use std::slice::{Iter, IterMut};
use std::iter::{Enumerate, Map, Extend, FromIterator, IntoIterator};
use std::ops::{Range, Index, IndexMut};

#[macro_export]
macro_rules! newtype_index {
    // ---- public rules ----

    // Use default constants
    ($name:ident) => (
        newtype_index!(
            // Leave out derives marker so we can use its absence to ensure it comes first
            @type         [$name]
            @max          [::std::u32::MAX]
            @debug_format ["{}"]);
    );

    // Define any constants
    ($name:ident { $($tokens:tt)+ }) => (
        newtype_index!(
            // Leave out derives marker so we can use its absence to ensure it comes first
            @type         [$name]
            @max          [::std::u32::MAX]
            @debug_format ["{}"]
                          $($tokens)+);
    );

    // ---- private rules ----

    // Base case, user-defined constants (if any) have already been defined
    (@derives      [$($derives:ident,)*]
     @pub          [$($pub:tt)*]
     @type         [$type:ident]
     @max          [$max:expr]
     @debug_format [$debug_format:tt]) => (
        #[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Deserialize, Serialize, $($derives),*)]
        pub struct $type($($pub)* u32);

        impl Idx for $type {
            #[inline]
            fn new(value: usize) -> Self {
                assert!(value < ($max) as usize);
                $type(value as u32)
            }

            #[inline]
            fn index(self) -> usize {
                self.0 as usize
            }
        }

        newtype_index!(
            @handle_debug
            @derives      [$($derives,)*]
            @type         [$type]
            @debug_format [$debug_format]);
    );

    // base case for handle_debug where format is custom. No Debug implementation is emitted.
    (@handle_debug
     @derives      [$($_derives:ident,)*]
     @type         [$type:ident]
     @debug_format [custom]) => ();

    // base case for handle_debug, no debug overrides found, so use default
    (@handle_debug
     @derives      []
     @type         [$type:ident]
     @debug_format [$debug_format:tt]) => (
        impl ::std::fmt::Debug for $type {
            fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                write!(fmt, $debug_format, self.0)
            }
        }
    );

    // Debug is requested for derive, don't generate any Debug implementation.
    (@handle_debug
     @derives      [Debug, $($derives:ident,)*]
     @type         [$type:ident]
     @debug_format [$debug_format:tt]) => ();

    // It's not Debug, so just pop it off the front of the derives stack and check the rest.
    (@handle_debug
     @derives      [$_derive:ident, $($derives:ident,)*]
     @type         [$type:ident]
     @debug_format [$debug_format:tt]) => (
        newtype_index!(
            @handle_debug
            @derives      [$($derives,)*]
            @type         [$type]
            @debug_format [$debug_format]);
    );

    // Handle the case where someone wants to make the internal field public
    (@type         [$type:ident]
     @max          [$max:expr]
     @debug_format [$debug_format:tt]
                   pub idx
                   $($tokens:tt)*) => (
        newtype_index!(
            @pub          [pub]
            @type         [$type]
            @max          [$max]
            @debug_format [$debug_format]
                          $($tokens)*);
    );

    // The default case is that the internal field is private
    (@type         [$type:ident]
     @max          [$max:expr]
     @debug_format [$debug_format:tt]
                   $($tokens:tt)*) => (
        newtype_index!(
            @pub          []
            @type         [$type]
            @max          [$max]
            @debug_format [$debug_format]
                          $($tokens)*);
    );

    // Append comma to end of derives list if it's missing
    (@pub          [$($pub:tt)*]
     @type         [$type:ident]
     @max          [$max:expr]
     @debug_format [$debug_format:tt]
                   derive [$($derives:ident),*]
                   $($tokens:tt)*) => (
        newtype_index!(
            @pub          [$($pub)*]
            @type         [$type]
            @max          [$max]
            @debug_format [$debug_format]
                          derive [$($derives,)*]
                          $($tokens)*);
    );

    // By not including the @derives marker in this list nor in the default args, we can force it
    // to come first if it exists. When encodable is custom, just use the derives list as-is.
    (@pub          [$($pub:tt)*]
     @type         [$type:ident]
     @max          [$max:expr]
     @debug_format [$debug_format:tt]
                   derive [$($derives:ident,)+]
                   ENCODABLE = custom
                   $($tokens:tt)*) => (
        newtype_index!(
            @derives      [$($derives,)+]
            @pub          [$($pub)*]
            @type         [$type]
            @max          [$max]
            @debug_format [$debug_format]
                          $($tokens)*);
    );

    // By not including the @derives marker in this list nor in the default args, we can force it
    // to come first if it exists. When encodable isn't custom, add serialization traits by default.
    (@pub          [$($pub:tt)*]
     @type         [$type:ident]
     @max          [$max:expr]
     @debug_format [$debug_format:tt]
                   derive [$($derives:ident,)+]
                   $($tokens:tt)*) => (
        newtype_index!(
            @derives      []
            @pub          [$($pub)*]
            @type         [$type]
            @max          [$max]
            @debug_format [$debug_format]
                          $($tokens)*);
    );

    // The case where no derives are added, but encodable is overridden. Don't
    // derive serialization traits
    (@pub          [$($pub:tt)*]
     @type         [$type:ident]
     @max          [$max:expr]
     @debug_format [$debug_format:tt]
                   ENCODABLE = custom
                   $($tokens:tt)*) => (
        newtype_index!(
            @derives      []
            @pub          [$($pub)*]
            @type         [$type]
            @max          [$max]
            @debug_format [$debug_format]
                          $($tokens)*);
    );

    // The case where no derives are added, add serialization derives by default
    (@pub          [$($pub:tt)*]
     @type         [$type:ident]
     @max          [$max:expr]
     @debug_format [$debug_format:tt]
                   $($tokens:tt)*) => (
        newtype_index!(
            @derives      []
            @pub          [$($pub)*]
            @type         [$type]
            @max          [$max]
            @debug_format [$debug_format]
                          $($tokens)*);
    );

    // Rewrite final without comma to one that includes comma
    (@derives      [$($derives:ident,)*]
     @pub          [$($pub:tt)*]
     @type         [$type:ident]
     @max          [$max:expr]
     @debug_format [$debug_format:tt]
                   $name:ident = $constant:expr) => (
        newtype_index!(
            @derives      [$($derives,)*]
            @pub          [$($pub)*]
            @type         [$type]
            @max          [$max]
            @debug_format [$debug_format]
                          $name = $constant,);
    );

    // Rewrite final const without comma to one that includes comma
    (@derives      [$($derives:ident,)*]
     @pub          [$($pub:tt)*]
     @type         [$type:ident]
     @max          [$_max:expr]
     @debug_format [$debug_format:tt]
                   $(#[doc = $doc:expr])*
                   const $name:ident = $constant:expr) => (
        newtype_index!(
            @derives      [$($derives,)*]
            @pub          [$($pub)*]
            @type         [$type]
            @max          [$max]
            @debug_format [$debug_format]
                          $(#[doc = $doc])* const $name = $constant,);
    );

    // Replace existing default for max
    (@derives      [$($derives:ident,)*]
     @pub          [$($pub:tt)*]
     @type         [$type:ident]
     @max          [$_max:expr]
     @debug_format [$debug_format:tt]
                   MAX = $max:expr,
                   $($tokens:tt)*) => (
        newtype_index!(
            @derives      [$($derives,)*]
            @pub          [$($pub)*]
            @type         [$type]
            @max          [$max]
            @debug_format [$debug_format]
                          $($tokens)*);
    );

    // Replace existing default for debug_format
    (@derives      [$($derives:ident,)*]
     @pub          [$($pub:tt)*]
     @type         [$type:ident]
     @max          [$max:expr]
     @debug_format [$_debug_format:tt]
                   DEBUG_FORMAT = $debug_format:tt,
                   $($tokens:tt)*) => (
        newtype_index!(
            @derives      [$($derives,)*]
            @pub          [$($pub)*]
            @type         [$type]
            @max          [$max]
            @debug_format [$debug_format]
                          $($tokens)*);
    );

    // Assign a user-defined constant
    (@derives      [$($derives:ident,)*]
     @pub          [$($pub:tt)*]
     @type         [$type:ident]
     @max          [$max:expr]
     @debug_format [$debug_format:tt]
                   $(#[doc = $doc:expr])*
                   const $name:ident = $constant:expr,
                   $($tokens:tt)*) => (
        $(#[doc = $doc])*
        pub const $name: $type = $type($constant);
        newtype_index!(
            @derives      [$($derives,)*]
            @pub          [$($pub)*]
            @type         [$type]
            @max          [$max]
            @debug_format [$debug_format]
                          $($tokens)*);
    );
}

pub trait Idx: Copy + Eq + Debug + 'static {
  fn new(v: usize) -> Self;
  fn index(self) -> usize;
}

pub type Enumerated<I, IT> = Map<Enumerate<IT>, IntoIdx<I>>;

#[derive(Clone, Eq, PartialEq, Hash, Deserialize, Serialize)]
pub struct IndexVec<I, T>
  where I: Idx,
{
  vec: Vec<T>,
  #[serde(skip)]
  _marker: PhantomData<Fn(&I)>,
}

impl<I, T> IndexVec<I, T>
  where I: Idx,
{
  pub fn new() -> Self { Default::default() }
  pub fn with_capacity(cap: usize) -> Self {
    IndexVec {
      vec: Vec::with_capacity(cap),
      _marker: PhantomData,
    }
  }
  pub fn from_elem<S>(elem: T, universe: &IndexVec<I, S>) -> Self
    where T: Clone,
  {
    IndexVec {
      vec: vec![elem; universe.len()],
      _marker: PhantomData,
    }
  }
  pub fn from_elem_n(elem: T, n: usize) -> Self
    where T: Clone,
  {
    IndexVec {
      vec: vec![elem; n],
      _marker: PhantomData,
    }
  }
  pub fn len(&self) -> usize { self.vec.len() }
  pub fn is_empty(&self) -> bool { self.vec.is_empty() }
  pub fn into_iter(self) -> IntoIter<T> { self.vec.into_iter() }
  pub fn iter(&self) -> Iter<T> { self.vec.iter() }
  pub fn last_idx(&self) -> Option<I> {
    if self.is_empty() {
      None
    } else {
      Some(I::new(self.len() - 1))
    }
  }
  pub fn next_idx(&self) -> I {
    I::new(self.len())
  }
  pub fn shrink_to_fit(&mut self) { self.vec.shrink_to_fit() }
  pub fn swap(&mut self, l: I, r: I) { self.vec.swap(l.index(), r.index()) }
  pub fn truncate(&mut self, s: usize) { self.vec.truncate(s) }
  pub fn get(&self, i: I) -> Option<&T> { self.vec.get(i.index()) }
  pub fn get_mut(&mut self, i: I) -> Option<&mut T> { self.vec.get_mut(i.index()) }

  pub fn last(&self) -> Option<&T> {
    self.vec.last()
  }
  pub fn last_mut(&mut self) -> Option<&mut T> {
    self.vec.last_mut()
  }

  pub fn reserve(&mut self, s: usize) {
    self.vec.reserve(s);
  }

  pub fn resize(&mut self, s: usize, v: T)
    where T: Clone,
  {
    self.vec.resize(s, v);
  }
  pub fn binary_search(&self, v: &T) -> Result<I, I>
    where T: Ord,
  {
    self.vec.binary_search(v)
      .map(I::new)
      .map_err(I::new)
  }
  pub fn push(&mut self, d: T) -> I {
    let idx = self.next_idx();
    self.vec.push(d);
    idx
  }

  pub fn push_with_idx<F>(&mut self, f: F) -> I
    where F: FnOnce(I) -> T,
  {
    let idx = self.next_idx();
    let d = f(idx);
    self.vec.push(d);
    idx
  }
}

// Whether `IndexVec` is `Send` depends only on the data,
// not the phantom data.
unsafe impl<I: Idx, T> Send for IndexVec<I, T> where T: Send {}

impl<I, T> Default for IndexVec<I, T>
  where I: Idx,
{
  fn default() -> Self {
    IndexVec {
      vec: vec![],
      _marker: PhantomData,
    }
  }
}

impl<I, T> Index<I> for IndexVec<I, T>
  where I: Idx,
{
  type Output = T;
  fn index(&self, i: I) -> &T { &self.vec[i.index()] }
}
impl<I, T> IndexMut<I> for IndexVec<I, T>
  where I: Idx,
{
  fn index_mut(&mut self, i: I) -> &mut T { &mut self.vec[i.index()] }
}
impl<I, T> Extend<T> for IndexVec<I, T>
  where I: Idx,
{
  fn extend<IT>(&mut self, iter: IT)
    where IT: IntoIterator<Item = T>,
  {
    self.vec.extend(iter)
  }
}
impl<I, T> FromIterator<T> for IndexVec<I, T>
  where I: Idx,
{
  fn from_iter<IT>(iter: IT) -> Self
    where IT: IntoIterator<Item = T>,
  {
    IndexVec {
      vec: FromIterator::from_iter(iter),
      _marker: PhantomData,
    }
  }
}
impl<I, T> IntoIterator for IndexVec<I, T>
  where I: Idx,
{
  type Item = T;
  type IntoIter = IntoIter<T>;

  fn into_iter(self) -> IntoIter<T> { self.into_iter() }
}
impl<'a, I, T> IntoIterator for &'a IndexVec<I, T>
  where I: Idx,
{
  type Item = &'a T;
  type IntoIter = Iter<'a, T>;

  fn into_iter(self) -> Self::IntoIter {
    self.iter()
  }
}

impl<I, T> Debug for IndexVec<I, T>
  where I: Idx,
        T: Debug,
{
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    Debug::fmt(&self.vec, f)
  }
}

pub struct IntoIdx<I>(PhantomData<Fn(&I)>)
  where I: Idx;
impl<I> Default for IntoIdx<I>
  where I: Idx,
{
  fn default() -> Self {
    IntoIdx(PhantomData)
  }
}
