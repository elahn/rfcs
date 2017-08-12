- Feature Name: delegation_of_implementation
- Start Date: 2015-12-12
- RFC PR: 
- Rust Issue: 

# Summary
[summary]: #summary

Syntax sugar to automatically implement a given trait `Tr` using a pre-existing type implementing `Tr`. The purpose is to improve code reuse in rust without damaging the orthogonality of existing concepts or adding new ones.

# Motivation
[motivation]: #motivation

Let's consider some existing pieces of code:
```rust
// from rust/src/test/run-pass/dropck_legal_cycles.rs
impl<'a> Hash for H<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state)
    }
}
```
We can see a recurring pattern where the implementation of a method only consists in applying the same method to a subfield or more generally to an expression containing `self`. Those are examples of the well known [composition pattern][object_composition]. It has a lot of advantages, but unfortunately requires writing boilerplate code again and again. In a classical OOP language we could also have opted for inheritance for similar cases. Inheritance comes with its own bunch of problems and limitations but at least it allows a straightforward form of code reuse: any subclass implicitly imports the public methods of its superclass(es).

One of the issues frequently mentioned when newcomers are learning Rust, is "I can do this easily in OOP, but is it even possible in Rust? And how the heck do I do it?" The lack of documentation/guides on this is a known issue and is being worked on, but it's not *just* a documentation issue.

OOP is used to solve real problems and if we want people to choose Rust in any of the domains they proliferate in, we need good solutions for those problems. As @withoutboats said:

> One aspect of inheritance-based polymorphism systems is that it is very easy to re-use code that has already been written as you are extending a system. The specific mechanism of re-use is connected to the more detrimental aspects of inheritance - the way it can result in ill-considered coupling and overly gregarious sharing of details which should be abstracted away.
> 
> To avoid the pitfalls that many inheritance-based languages have fallen into, Rust has avoided that form of polymorphism entirely, preferring instead a combination of behaviorally constrained parametric polymorphism (traits and generics) and straightforward type composition.
> 
> Avoiding inheritance has resulted in two major costs for users of Rust:
> 
> - There are patterns enabled by inheritance which have no clean equivalent in Rust.
> - The forms of abstraction Rust suggests can result in more boilerplate and less convenient code re-use than inheritance.
> 
> We've focused a lot of attention on resolving the first problem, which is what has driven the efforts around specialization and the like. But there hasn't been nearly as much attention at resolving the second problem. This RFC is aimed squarely at that problem, so thanks @contactomorph for considering that question.

Efficient code reuse is about making us more productive, not just in terms of typing less, which is nice, but being able to clearly express *intent* in our code, making it easier to read, understand, refactor and prototype quickly. It also enables DRY and is in-line with the [2017 Roadmap](https://github.com/rust-lang/rust-roadmap): 

> In short, productivity should be a core value of Rust. By the end of 2017, let's try to earn the slogan: 
> 
> - Rust: fast, reliable, productive—pick three.

Rust has no inheritance (yet) and as a result composition is an even more interesting pattern for factoring code than in other languages. In fact it is already used in many places. Some (approximate) figures:

Project           | Occurences of "delegating methods" |
------------------| ---------------------------------- |
rust-lang/rust    | 845                                |
rust-lang/cargo   | 38                                 |
servo/servo       | 314                                |

By providing syntax sugar for the composition pattern, it can remain/become a privileged tool for code reuse while being as terse as the inheritance-based equivalent. Related discussions:
* [pre-rfc][pre_rfc]
* [some related reddit thread][comp_over_inh] (I didn't participate in)

[pre_rfc]: https://internals.rust-lang.org/t/syntactic-sugar-for-delegation-of-implementation/2633
[comp_over_inh]: https://www.reddit.com/r/rust/comments/372mqw/how_do_i_composition_over_inheritance/
[object_composition]: https://en.wikipedia.org/wiki/Composition_over_inheritance

# Guide-level explanation
[guide]: #guide

In Rust we prefer composition over inheritence for code reuse. For common cases, we make this convenient with delegation syntax sugar.

# Reference-level explanation
[reference]: #reference

Delegation is syntax sugar for common cases of the composition pattern. Given the code:

```rust
impl<'a> Hash for H<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state)
    }
}
```

## Delegate a trait impl to a struct field

Syntax sugar to delegate all trait methods to a struct member with a type which implements the trait:

```rust
impl<'a> Hash for H<'a> => self.name;
```

Let's compare this with inheritance. The *delegating type* (`H<'a>` in the first example) implicitely "inherits" methods (`hash`) of the *delegated trait* (`Hash`) from the *surrogate type* (`&'static str` which is the type of the *delegating expression* `self.name`) like a subclass inherits methods from its superclass(es). A fundamental difference is that the delegating type is not a subtype of the surrogate type in the sense of Liskov. There is no external link between the types. The surrogate may even be less visible than the delegating type. Another difference is that the developer has a total control on which part of the surrogate type to reuse whereas class hierarchy forces him/her to import the entire public interface of the superclass (this is because a superclass plays two roles: the role of the surrogate type and the role of the delegated trait).


## Don't delegate this trait method, use my implementation

Any trait methods added to the `impl` block will be used and will not be delegated. (*partial delegation*)

```rust
impl<'a> Hash for H<'a> => self.name {
    //method definitions that will not be delegated go here
}
```

## Method delegation

Method delegation can be useful for:
- explicit control and/or it's undesirable new default trait methods automatically implemented on the type
- splitting the delegation across multiple fields, so some trait methods are delegated to field_a, some to field_b, etc. (*mixed partial delegation*)

For single method traits, these delegations could be expressed on a single line if the style is deemed acceptable.

```rust
impl<'a> Hash for H<'a> {
    fn hash = self.name.hash();
}
```
The parenthesis at the end of the method name, e.g. `encode()` are left empty and are filled in with the arguments passed in. They are there to disambiguate between field and method names, which exist in separate namespaces. Without parenthesis, code like this could be confusing:

```rust
struct MyStruct {
    method: u8
}

impl MyStruct {
    fn method(&self) -> u8 { 5 }
}

struct OuterType(MyStruct);

// This would refer to the method, not the field:
impl MyTrait for OuterType { fn method = self.0.method; }
```

If we want to delegate some but not all methods of a trait, the remaining method definitions must be supplied to fully implement the trait on the type.


## Delegating inherent methods - Privacy and Encapsulation

While we imagine delegating traits will usually be the best practice, it could be valuable to do so with inherent `impl`s as well. This makes the language more consistent and preempts the questions: "Why can't I do this directly on my type? Why do I have to define a trait?"

Let's take this opportunity to explore privacy and encapsulation using delegation, which also applies to delegating traits.

```rust
struct AppendOnlyVec<T> (Vec<T>);

impl<T> AppendOnlyVec<T> {
    delegate fn push to self.0;
    // other meaningful methods
}
```
This is an example of a "restricted type" without using a trait. We can easily delegate the methods we want to the inner Vec, thereby restricting access to functionality we don't want to expose.


## Delegating a trait to an arbitrary expression

```rust
// from rust/src/libsyntax/attr.rs at commit 5553901 on 26 Jul 2016
impl AttrMetaMethods for Attribute {
    fn check_name(&self, name: &str) -> bool {
        let matches = name == &self.name()[..];
        if matches {
            mark_used(self);
        }
        matches
    }
    fn name(&self) -> InternedString { self.meta().name() }
    fn value_str(&self) -> Option<InternedString> {
        self.meta().value_str()
    }
    fn meta_item_list(&self) -> Option<&[P<MetaItem>]> {
        self.meta().meta_item_list()
    }

    fn is_word(&self) -> bool { self.meta().is_word() }

    fn span(&self) -> Span { self.meta().span() }
}

impl AttrMetaMethods for MetaItem {
    // same method signatures
}

impl AttributeMethods for Attribute {
    /// Extract the MetaItem from inside this Attribute.
    fn meta(&self) -> &MetaItem {
        &self.node.value
    }
}
```
Let's delegate all methods of the trait AttrMetaMethods to the expression `self.meta()`, except `check_name()`:
```rust
impl AttrMetaMethods for Attribute => self.meta() {
    
    fn check_name(&self, name: &str) -> bool {
        let matches = name == &self.name()[..];
        if matches {
            mark_used(self);
        }
        matches
    }    
}
```
Code is generated by appending `.trait_method_name()` to the *delegating expression* `self.meta()`. In the example, `&self` is the only parameter being forwarded.


## Forwarding parameters

When generating code, the function signature is copied from the function being delegated to.

For methods, the first parameter is `self`, `&self` or `&mut self`. In all cases, the generated code contains `self`, just as if we had written the delegation manually.

Subsequent parameters are passed through in the generated code. If we didn't supply a defintion for `fn check_name()` above, this code would be generated:

```rust
fn check_name(&self, name: &str) -> bool {
    self.meta().check_name(&name)
}    
```

## Delegating expressions and the type of `self`

In Rust the actual type of the `self` parameter can vary depending on the method in the trait. It is either `Self`, `&mut Self` or `&Self`. This means that:
* each method locally defines the type of `self`;
* for a delegating expression to be valid it must type-check with all delegated methods.

Because inherited mutability apply to expressions like `self.2` and `self.my_field`, they may be used to handle methods where `self` have different type. Furthermore as [auto-referencing and auto-dereferencing][auto] is implicitely applied on method call, the return type of the delegating expression may be transformed if possible to match the expected type of `self`. However for a more complex delegating expression that contains function or method calls with non-convertible return types, such flexibility may not be available. In this case *mixed partial delegation* can be used to handle differently the groups of methods with respective parameter `self`, `&self` and `&mut self` if needed.

The proposed rule for validity of a delegating expression `expression` when handling method `my_method` in trait `Trait` is the same as the validity of the plain equivalent code:
```rust
impl Trait for MyType {
    fn my_method(self_parameter, other_parameters… ) {
        expression.my_method(other_parameters… )
    }
    …
}
```
`expression` may contain several occurences of `self`, or none in the case of associated functions.

In case the code above contains a type error, the compiler must provide an error message indicating that method `my_method` from trait `Trait` cannot be delegated using `expression` (for example because `my_method` is expecting a `&mut self` but `expression` is returning a result of type `&MySurrogateType`).

[auto]: https://doc.rust-lang.org/nomicon/dot-operator.html


## Restrictions on delegating expressions

None.


## Associated items

Associated types and constants (or anything that is not a `fn`) are never delegated. Whenever a trait declares such items, implementations must define their actual values.


## Delegating to other types

Delegating types and surrogate types might be of any kind (structs, tuples, enums, arrays, lambdas, ...) provided it makes sense. An illustrative example with enums:
```rust
enum HTMLColor { White, Silver, Gray, Black,
	Red, Maroon, Yellow, Olive,
	Lime, Green, Aqua, Teal,
	Blue, Navy, Fuchsia, Purple };

impl Coordinates for HTMLColor {
	fn get_red(&self) -> f32 { … }
	fn get_green(&self) -> f32 { … }
	fn get_blue(&self) -> f32 { … }
	fn get_hue(&self) -> f32 { … }
	fn get_saturation(&self) -> f32 { … }
	fn get_brightness(&self) -> f32 { … }
}

enum ThreeBitColor { Black, Blue, Green, Cyan,
	Red, Magenta, Yellow, White };

fn to_html_color(color: &ThreeBitColor) -> HTMLColor { … }

impl Coordinates for ThreeBitColor {
    use to_html_color(&self);
}
```

## Possible extensions

Delegation can be extended in a certain number of directions to handle other specific cases. Propositions below are simply invitation for debate and are not part of the main proposition.

### Delegation for other `Self` parameters 

If method `my_method` contains other parameters with type `Self`, `&Self` or `&mut Self`, delegation may still be possible provided that the delegating expression is simultaneously compatible with all those types. In this case for each such parameter `p` the delegating expression can be transformed by replacing all occurences of `self` by `p`. The resulting expression is then inserted at the position where parameter `p` is normally expected. For example
```rust
// from rust/src/libcollections/btree/map.rs
impl<K: PartialOrd, V: PartialOrd> PartialOrd for BTreeMap<K, V> {
    fn partial_cmp(&self, other: &BTreeMap<K, V>) -> Option<Ordering> {
        self.iter().partial_cmp(other.iter())
    }
}
```
could become 
```rust
impl<K: PartialOrd, V: PartialOrd> PartialOrd for BTreeMap<K, V> {
    use self.iter();
}
```
where the delegating expression `self.iter()` is implicitely transformed into `other.iter()` in order to handle the second parameter of method `partial_cmp`.

### Inverse delegating expressions

Can we handle cases as this one
```rust
// from servo/components/layout/block.rs
impl<T: Clone> Clone for BinaryHeap<T> {
    fn clone(&self) -> Self {
        BinaryHeap { data: self.data.clone() }
    }

    fn clone_from(&mut self, source: &Self) {
        self.data.clone_from(&source.data);
    }
}
```
where `Self` is used as a return type? Yes but we need a second expression for that.
```rust
impl<T: Clone> Clone for BinaryHeap<T> {
    use self.data, BinaryHeap { data: super };
}
```
Here the `super` keyword corresponds to an instance of the surrogate type. It is the symmetric of `self`. The whole expression must have type `Self`. Both direct and inverse delegating expressions may be given at the same time or possibly just one of them if only one conversion is needed.

### Combined delegation

It would be nice if delegation could be combined for multiple traits so that
```rust
// from cargo/src/cargo/core/package_id.rs 
impl PartialEq for PackageId {
    fn eq(&self, other: &PackageId) -> bool {
        (*self.inner).eq(&*other.inner)
    }
}
impl PartialOrd for PackageId {
    fn partial_cmp(&self, other: &PackageId) -> Option<Ordering> {
        (*self.inner).partial_cmp(&*other.inner)
    }
}
impl Ord for PackageId {
    fn cmp(&self, other: &PackageId) -> Ordering {
        (*self.inner).cmp(&*other.inner)
    }
}
```
could be reduced to:
```rust
impl PartialEq + PartialOrd + Ord for PackageId {
    use &*self.inner;
}
```

### Trait-free delegation

Sometimes implementations are trait-free but the same pattern is found like in
```rust
// from rust/src/librustc/middle/mem_categorization.rs
impl<'t, 'a,'tcx> MemCategorizationContext<'t, 'a, 'tcx> {
    
    …
    
    fn node_ty(&self, id: ast::NodeId) -> McResult<Ty<'tcx>> {
        self.typer.node_ty(id)
    }
}
```
Here we have no trait to delegate but the same method signatures are reused and semantically the situation is close to a trait-based implementation. A simple possibility could be to introduce a new trait. An alternative is to allow delegation even without traits but with the name of the method becoming mandatory:
```rust
impl<'t, 'a,'tcx> MemCategorizationContext<'t, 'a, 'tcx> {
    use self.typer for node_ty;
}
```

### Renaming delegation

Maybe the method you want to delegate does not come from the same trait, it also has a distinct original name but it just happens to have the same signature.

```rust
impl A { 
    fn do_something(&mut self, predicate: P) -> Option<i32> where P: FnMut(&str) -> bool { … }
    fn do_something_else(&self) -> i32 { … }
    …
}

trait Tr {
    fn select_first(&mut self, predicate: P) -> Option<i32> where P: FnMut(&str) -> bool;
    fn count(&self) -> i32;
    …
}

fun to_a(b : &B) -> &A { … }

impl Tr for B {
    // here we are mapping distinct methods with same signatures:
    // B::select_first <- A::do_something
    // B::count <- A::do_something_else
    use do_something, do_something_else in to_a(self) for select_first, count;
    …
}
```
The general syntax becomes:
```rust
use list_of_surrogate_methods in delegating_expression for list_of_delegated_methods;
```
and the previous syntax is now just a shortcut for:
```rust
use list_of_delegated_methods in delegating_expression for list_of_delegated_methods;
```

### More complex delegation

`Self` can also appear inside more complex parameter/result types like `Option<Self>`, `Box<Self>` or `&[Self]`. When we have higher-kinded types in Rust a partial solution based on [functor types][functors] may been possible.

[functors]: https://wiki.haskell.org/Functor

### Value-dependent surrogate type

Let's consider a new example:
```rust
enum TextBoxContent { Number(f64), String(&'static str) }

impl Hash for TextBoxContent {
    use ??? ; // how to delegate?
}
```
It seems that in theory we should be able to delegate meaningfully given that for any value of `TextBoxContent` there is an obvious existing implementation for `Hash`. The problem is we cannot select a **single** surrogate type. The actual surrogate type should indeed be chosen based on the runtime value of `Self`. To handle this case I slightly modify the delegation syntax by using a variation of blaenk's proposition: `impl Tr for B { use delegatingExpression.impl; }`. Now this new syntax could be extended to solve our current issue:
```rust
impl Hash for TextBoxContent {
    use (match self { Number(n) => n.impl, String(s) => s.impl });
}
```
Here the delegating expression can contain several branches that does not need to unify from a type perspective. The `.impl` syntax should be replaced by a call to the actual delegated method. Note that although this pattern may occur naturally with enums it can again apply to any kind of types:
```rust
impl Tr for BStruct {
    use (if self.condition { self.field1.impl } else { self.field2.impl });
}
```
However this kind of delegation for value-dependent surrogate types has a limitation: it does not work for methods with multiple `Self` parameters. Indeed there is no guarantee the runtime values for different parameters will select the same branch and then define a consistent surrogate.

# Drawbacks
[drawbacks]: #drawbacks

* It creates some implicit code reuse. This is an intended feature but it could also be considered as dangerous. Modifying traits and surrogate types may automatically import new methods in delegating types with no compiler warning even in cases it is not appropriate (but this issue is the same as modifying a superclass in OOP).
* The benefit may be considered limited for one-method traits.


# Alternatives
[alternatives]: #alternatives

## OOP inheritance

As mentioned before, inheritance can handle similar cases with the advantage its concepts and mechanisms are well known. But with some drawbacks:
* Multiple inheritance is possible but to my knowledge no serious proposition has been made for Rust and I doubt anyone wants to end up with a system as complex and tricky as C++ inheritance (whereas delegation is **naturally multiple delegation**)
* As said before inheritance mixes orthogonal concepts (code reuse and subtyping) and does not allow fine grain control over which part of the superclass interface is inherited.

## Multiple derefs

Some people noticed a similarity with trait `Deref`. A main limitation is that you can only deref to a single type. However one could imagine implementing multiple derefs by providing the target type as a generic parameter (`Deref<A>`) rather than as an associated type. But again you can find limitations:
* As for inheritance visibility control is impossible: if `B` can be derefed to `A` then the entire public interface of `A` is accessible.
* `Deref` only offers a superficial similarity. If `A` implements trait `Tr`, instances of `B` can sometimes be used where `Tr` is expected but as a counter example a `&[B]` slice is not assignable to `fn f(t: &[T]) where T : Tr`. Derefs do not interact nicely with bounded generic parameters.

## Compiler plugin

I was suggested to write a compiler plugin. But I was also told that [type information is not accessible][type_information] (unless you can annotate the delegated trait yourself, which implies you must own it). The problem is that delegation requires to analyse the signatures of methods to be delegated and this is as far as I know not currently possible.

[type_information]: http://stackoverflow.com/questions/32641466/when-writing-a-syntax-extension-can-i-look-up-information-about-types-other-tha

## Do nothing

In the end, it is syntactic sugar. It just improves the ease of expression, not the capacity to express more concepts. Some simple cases may be handled with deref, others with trait default methods.

One of my concerns is that the arrival of inheritance in Rust may encourage bad habits. Developers are lazy and DRY principle dissuades them from writing repetitive code. The temptation may be strong to overuse inheritance in situations where only code reuse is required (resulting in unnecessary subtyping hierarchy and uncontrolled interface exposure).

# Unresolved questions
[unresolved]: #unresolved-questions

@cramertj has requested the desired syntax be selected before accepting this RFC, since it is entirely syntax sugar. Here are some of the suggestions so far, let the bikeshedding begin!

```rust
- #[derive(PartialEq(“self.1.abs()”)]
- struct MyType { #[delegate(MyTrait)] field: FieldType }
- impl MyTrait for MyType as self.0
- impl MyTrait for MyType via self.0
- use impl self.name
- use self.name impl
- impl MyTrait for MyType { fn_name use self.field.fn_name }
- impl MyType { delegate fn_name to self.field; delegate Vec::* to self.field; }
- impl MyTrait for MyType => self.0;
- impl MyTrait for MyType { fn method = self.field.method; }
```

## Delegate a trait impl to a struct field

### Original RFC Syntax

```rust
impl<'a> Hash for H<'a> {
    use self.name;
}
```

### Alternative Syntax 1

```rust
impl<'a> Hash for H<'a> { delegate to self.name; }
```

### Alternative Syntax 2

```rust
impl<'a> H<'a> {
    delegate Hash to self.name;
}
```

### Alternative Syntax 3

```rust
impl<'a> H<'a> {
    delegate Hash::* to self.name;
}
```

### Alternative Syntax 4

```rust
impl<'a> Hash for H<'a> use self.name {}
```

### Alternative Syntax 5

```rust
#[delegate(self.name)]
impl<'a> Hash for H<'a> {}
```

### Alternative Syntax 6

```rust
#[delegate(field = name)]
impl<'a> Hash for H<'a> {}
```

### Alternative Syntax 7

```rust
// on the type itself
struct H<'a> {
    #[delegate(Hash)]
    name : &'static str,
    …
}
```

### Alternative Syntax 8

```rust
// on the type itself
#[derive(Hash("self.name"))]
struct H<'a> {
    name : &'static str,
    …
}
```

### Alternative Syntax 9

```rust
impl<'a> Hash for H<'a> as self.name;
```

### Alternative Syntax 10

```rust
impl<'a> Hash for H<'a> via self.name;
```


## Don't delegate this trait method, use my implementation

Syntaxes 9 and 10 would have blocks added instead of a semicolon, like in the reference-level explanation:

```rust
impl<'a> Hash for H<'a> => self.name {
    //method definitions that will not be delegated go here
}
```

## Method delegation

### Original RFC Syntax

```rust
impl<'a> Hash for H<'a> {
    use self.name for hash;
}
```

### Alternative Syntax 1

```rust
impl<'a> Hash for H<'a> {
    delegate fn hash() to self.name;
}
```

### Alternative Syntax 2

```rust
impl<'a> H<'a> {
    delegate Hash::hash() to self.name.hash();
}
```

### Alternative Syntax 3

```rust
impl<'a> H<'a> {
    delegate Hash::hash to self.name;
}
```

### Alternative Syntax 4

Same as Syntax 1.

### Alternative Syntax 5

<!-- ```rust
#[delegate(self.name)]
impl<'a> Hash for H<'a> {}
``` -->

### Alternative Syntax 6

<!-- ```rust
#[delegate(field = name)]
impl<'a> Hash for H<'a> {}
``` -->

### Alternative Syntax 7

<!-- ```rust
// on the type itself
struct H<'a> {
    #[delegate(Hash)]
    name : &'static str,
    …
}
``` -->

### Alternative Syntax 8

<!-- ```rust
// on the type itself
#[derive(Hash("self.name"))]
struct H<'a> {
    name : &'static str,
    …
}
``` -->

### Alternative Syntax 9

```rust
impl<'a> Hash for H<'a> {
    fn hash as self.name;
}
```

### Alternative Syntax 10

```rust
impl<'a> Hash for H<'a> {
    fn hash via self.name;
}
```
