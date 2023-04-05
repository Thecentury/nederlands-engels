module NederlandsEngels.Machines

(*--------------------------------------------------------------------------------------------------------------------*)

type Step<'o, 'm> =
    | Halt
    | Transition of 'o * 'm

type Machine<'i, 'o> = Machine of ('i -> Step<'o, Machine<'i,'o>>)

(*--------------------------------------------------------------------------------------------------------------------*)

module Machine =

    /// Offers one input to the state machine.
    let step s (Machine trans) = trans s
    
    /// Constructs a state machine which cannot perform any transitions.
    let halt () = Machine ^ constant Halt
    
    /// Constructs a state machine from a transition function which may require actions.
    let machine = Machine

    /// Constructs a state machine which for any input outputs the given value exactly once and halts.
    let singleton a = machine ^ constant ^ Transition (a, halt ())

    /// Combines two state machines sequentially.
    let rec combine l r =
        let go s = function
            | Transition (o, l') -> Transition (o, (combine l' r))
            | Halt -> step s r
        Machine ^ fun s -> step s l |> go s

    let rec map f m = Machine ^ fun i ->
        match step i m with
        | Halt -> Halt
        | Transition (e, m') -> Transition (f e, map f m')

    let rec mapDest f m = Machine ^ fun i ->
        match step i m with
        | Halt -> Halt
        | Transition (e, m') -> Transition (e, mapDest f (f m'))

    /// Returns a state machine which, for each output produced by the given state machine, behaves like the machine
    /// derived from that output by the given function.
    let rec bind f m =
        let go s = function
            | Halt -> Halt
            | Transition (o, m') ->
                let switch = function
                | Halt -> Halt
                | Transition (e, sm) -> Transition (e, combine sm (bind f m'))
                switch ^ step s (f o)
        Machine ^ fun s -> step s m |> go s

    let join mm = bind id mm

    /// Returns a state machine which replaces the halting of the given state machine with itself.
    let rec loop m0 =
        let rec go m = Machine ^ fun i ->
            step i m |> function
            | Transition (o, m1) -> Transition (o, go m1)
            | Halt -> step i (loop m0)
        go m0

    let alt m1 m2 = Machine ^ fun s ->
        match step s m1 with
        | Halt -> step s m2
        | Transition (o, m1') -> Transition (o, m1')

    let rec par combine m1 m2 = Machine ^ fun i -> // Essentially, output type should form a semiring.
        match step i m1, step i m2 with
        | Halt, Halt -> Halt
        | Halt, Transition (a, m2') -> Transition (a, m2')
        | Transition (a, m1'), Halt -> Transition (a, m1')
        | Transition (a, m1'), Transition (b, m2') -> Transition (combine a b, par combine m1' m2')    

    // (Monoid o) => M i o -> M i o -> M i o
    let parAlways zero combine m1 m2 = // Essentially, output type should form a monoid.
        let rec go mm1 mm2 = Machine ^ fun i ->
            match step i mm1, step i mm2 with
            | Halt, Halt -> Transition (zero, go m1 m2)
            | Halt, Transition (a, m2') -> Transition (a, go m1 m2')
            | Transition (a, m1'), Halt -> Transition (a, go m1' m2)
            | Transition (a, m1'), Transition (b, m2') -> Transition (combine a b, go m1' m2')
        go m1 m2
            
(*--------------------------------------------------------------------------------------------------------------------*)

type Machine<'i,'o> with
    static member (+) (l, r) = Machine.combine l r
    static member (<!>) (f, m) = Machine.map f m
    static member (>>=) (m, f) = Machine.bind f m
    static member ( *> ) (m1, m2) = m1 |> Machine.bind (fun _ -> m2)
    static member (<|>) (m1, m2) = Machine.alt m1 m2
    static member (<|>) (m1, Lazy m2) = Machine.alt m1 m2

(*--------------------------------------------------------------------------------------------------------------------*)

type MachineBuilder () = 
    member b.Bind(m, f) = Machine.bind f m
    member b.Return(v) = Machine.singleton v
    member b.ReturnFrom(m) = m
    member b.Combine(m1, m2) = m1 |> Machine.bind (fun _ -> m2)
    member b.Zero() = Machine.halt ()

let machine = MachineBuilder()
