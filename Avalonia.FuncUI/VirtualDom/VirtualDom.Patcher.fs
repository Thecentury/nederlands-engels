namespace Avalonia.FuncUI.VirtualDom

open Avalonia.FuncUI.VirtualDom.Delta

module internal rec Patcher =
    open System
    open System.Collections
    open System.Collections.Concurrent
    open Avalonia.Controls
    open Avalonia
    open Avalonia.FuncUI.VirtualDom.Delta
    open Avalonia.FuncUI.Library
    open Avalonia.FuncUI.Types
    open System.Threading

    let private shouldPatch (value: obj, viewElement: ViewDelta) =
         value <> null
         && value.GetType() = viewElement.ViewType
         && not viewElement.KeyDidChange

    let private patchSubscription (view: Control, attr: SubscriptionDelta) : unit =
        let subscriptions =
            match ViewMetaData.GetViewSubscriptions(view) with
            | null ->
                let dict = ConcurrentDictionary<_, _>()
                ViewMetaData.SetViewSubscriptions(view, dict)
                dict
            | value -> value

        match attr.Func with
        // add or update
        | Some handler ->
            let cts = attr.Subscribe(view, handler)

            let addFactory = Func<string, CancellationTokenSource>(fun key -> cts)

            let updateFactory = Func<string, CancellationTokenSource, CancellationTokenSource>(fun key old_cts ->
                old_cts.Cancel()
                cts
            )

            subscriptions.AddOrUpdate(attr.UniqueName, addFactory, updateFactory) |> ignore

        // remove
        | None ->
            let hasValue, value = subscriptions.TryGetValue(attr.UniqueName)
            if hasValue then
                value.Cancel()
                subscriptions.TryRemove(attr.UniqueName) |> ignore

    let internal patchProperty (view: AvaloniaObject, attr: PropertyDelta) : unit =
        match attr.Accessor with
        | Accessor.AvaloniaProperty avaloniaProperty ->
            match attr.Value with
            | Some value ->
                view.SetValue(avaloniaProperty, value)
                |> ignore
            | None ->
                match attr.DefaultValueFactory with
                | ValueNone ->
                    view.ClearValue(avaloniaProperty)
                | ValueSome factory ->
                    let value = factory()
                    view.SetValue(avaloniaProperty, value)
                    |> ignore

        | Accessor.InstanceProperty instanceProperty ->
            let value =
                match attr.Value with
                | Some value -> value
                | None ->
                    match attr.DefaultValueFactory with
                    | ValueSome factory -> factory()
                    | ValueNone ->
                        // TODO: get rid of reflection here
                        let propertyInfo = view.GetType().GetProperty(instanceProperty.Name)

                        match propertyInfo.PropertyType.IsValueType with
                        | true -> Activator.CreateInstance(propertyInfo.PropertyType)
                        | false -> null

            match instanceProperty.Setter with
            | ValueSome setter -> setter (view, value)
            | ValueNone _ -> failwithf "instance property ('%s') has no setter. " instanceProperty.Name

    let private patchContentMultiple (view: AvaloniaObject, accessor: Accessor, delta: ViewDelta list) : unit =
        (* often lists only have a get accessor *)
        let patch_IList (collection: IList) : unit =
            if List.isEmpty delta then
                collection.Clear()
            else
                delta |> Seq.iteri (fun index viewElement ->
                    // try patch / reuse
                    if index + 1 <= collection.Count then
                        let item = collection.[index]

                        if shouldPatch (item, viewElement) then
                            // patch
                            match item with
                            | :? AvaloniaObject as control -> patch(control, viewElement)
                            | _ ->
                                // replace
                                let newItem = Patcher.create viewElement
                                collection.[index] <- newItem
                        else
                            // replace
                            let newItem = Patcher.create viewElement
                            collection.[index] <- newItem
                    else
                        // create
                        let newItem = Patcher.create viewElement
                        collection.Add(newItem) |> ignore
                )

                while delta.Length < collection.Count do
                    collection.RemoveAt (collection.Count - 1)

        (* read only, so there must be a get accessor *)
        let patch_IEnumerable (collection: IEnumerable) : IEnumerable =
            let newList =
                collection
                |> Seq.cast<obj>
                |> ResizeArray

            patch_IList newList

            (newList :> IEnumerable)

        let patch (getValue: (unit -> obj) voption, setValue: (obj -> unit) voption) =
            let value =
                match getValue with
                | ValueSome get -> get()
                | ValueNone -> failwith "accessor must have a getter"

            match value with
            | :? IList as collection ->
                patch_IList collection

            | :? IEnumerable as enumerable ->
                match setValue with
                | ValueSome set -> set (patch_IEnumerable enumerable)
                | ValueNone -> failwith "accessor must have a setter"

            | _ -> raise (Exception("type does not implement IEnumerable or IList. This is required for view patching"))

        match accessor with
        | Accessor.InstanceProperty instanceProperty ->
            let getter =
                match instanceProperty.Getter with
                | ValueSome getter -> ValueSome (fun () -> getter(view))
                | ValueNone -> ValueNone

            let setter =
                match instanceProperty.Setter with
                | ValueSome setter -> ValueSome (fun value -> setter(view, value))
                | ValueNone -> ValueNone

            patch (getter ,setter)

        | Accessor.AvaloniaProperty property ->
            let getter = ValueSome (fun () -> view.GetValue(property))
            let setter = ValueSome (fun obj -> view.SetValue(property, obj) |> ignore)
            patch (getter, setter)

    let private patchContentSingle (view: AvaloniaObject, accessor: Accessor, viewElement: ViewDelta option) : unit =

        let patch_avalonia (property: AvaloniaProperty) =
            match viewElement with
            | Some viewElement ->
                let value = view.GetValue(property)

                if shouldPatch (value, viewElement) then
                    Patcher.patch(value :?> AvaloniaObject, viewElement)
                else
                    let createdControl = Patcher.create viewElement
                    view.SetValue(property, createdControl)
                    |> ignore
            | None ->
                view.ClearValue(property)

        let patch_instance (property: PropertyAccessor) =
            match viewElement with
            | Some viewElement ->
                let value =
                    match property.Getter with
                    | ValueSome getter -> getter(view)
                    | _ -> failwith "Property Accessor needs a getter"

                if shouldPatch (value, viewElement) then
                    Patcher.patch(value :?> AvaloniaObject, viewElement)
                else
                    let createdControl = Patcher.create(viewElement)

                    match property.Setter with
                    | ValueSome setter -> setter(view, createdControl)
                    | _ -> failwith "Property Accessor needs a setter"
            | None ->
                match property.Setter with
                | ValueSome setter -> setter(view, null)
                | _ -> failwith "Property Accessor needs a setter"

        match accessor with
        | Accessor.InstanceProperty instanceProperty -> patch_instance instanceProperty
        | Accessor.AvaloniaProperty property -> patch_avalonia property

    let private patchContent (view: AvaloniaObject, attr: ContentDelta) : unit =
        match attr.Content with
        | ViewContentDelta.Single single ->
            patchContentSingle (view, attr.Accessor, single)
        | ViewContentDelta.Multiple multiple ->
            patchContentMultiple (view, attr.Accessor, multiple)

    let patch (view: AvaloniaObject, viewElement: ViewDelta) : unit =
        for attr in viewElement.Attrs do
            match attr with
            | AttrDelta.Property property -> patchProperty (view, property)
            | AttrDelta.Content content -> patchContent (view, content)
            | AttrDelta.Subscription subscription ->
                match view with
                | :? Control as control ->
                    patchSubscription (control, subscription)
                | _ -> failwith "Only controls can have subscriptions"
            | AttrDelta.SetupFunction _ ->
                // setup/init functions are only called on control creation
                ()

    let create (viewElement: ViewDelta) : AvaloniaObject =
        let control =
            if viewElement.ConstructorArgs <> null && viewElement.ConstructorArgs.Length > 0 then
                (viewElement.ViewType, viewElement.ConstructorArgs)
                |> Activator.CreateInstance
                |> Utils.cast<AvaloniaObject>
            else
                viewElement.ViewType
                |> Activator.CreateInstance
                |> Utils.cast<AvaloniaObject>

        match viewElement.Outlet with
        | ValueSome outlet -> outlet control
        | ValueNone -> ()

        control.SetValue(ViewMetaData.ViewIdProperty, Guid.NewGuid()) |> ignore

        for attr in viewElement.Attrs do
            match attr with
            | AttrDelta.Content content -> Patcher.patchContent (control, content)
            | AttrDelta.Subscription s -> Patcher.patchSubscription ((control :?> Control), s)
            | AttrDelta.Property property -> Patcher.patchProperty (control, property)
            | AttrDelta.SetupFunction setupFunction -> setupFunction.Function(control)

        control