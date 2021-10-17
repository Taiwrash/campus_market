//the useed types in the contract
type property_supply = { id: int; buyer_address : address ; property_price : tez ; sale_status : bool ; out_of_stock : bool; property_name : string ; property_description : string }
type props_storage = (nat, property_supply) map
type return = operation list * props_storage
type property_id = nat


//types that are required for property transfer function 
type transfer_destination =
[@layout:comb]
{
  to_ : address;
  property_id : property_id;
  amount : nat;
}
 
type transfer =
[@layout:comb]
{
  from_ : address;
  txs : transfer_destination list;
}

//address to recieve money from property sales
let publisher_address : address = ("tz1bKuQER7Kadk7RZANAFxTmCznCurHCfpcg" : address)


let update_item(props_kind_index,property_kind,storage:property_id*property_supply*props_storage): props_storage =

  if (property_kind.sale_status = true) then
    let props_storage: props_storage = Map.update
      props_kind_index
      (Some { property_kind with out_of_stock = true })
      props_storage
    in
    props_storage



// main function
let main (props_kind_index, props_storage : nat * props_storage) : return =
    //checks if the property exist
  let property_kind : property_supply =
    match Map.find_opt (props_kind_index) props_storage with
    | Some k -> k
    | None -> (failwith "Sorry, We do not stock the requested property!" : property_supply)
  in

  // Check if the property is on sale  
  let () = if property_kind.sale_status = true then
    failwith "Sorry, This property is not for sale!"
  in

  // Check if offer is enough to cover price of property.  
  let () = if Tezos.amount < property_kind.property_price then
    failwith "Sorry, This property is worth more tha that!"
  in

 // Check if the property is in stock.
  let () = if property_kind.out_of_stock = false then
    failwith "Sorry, This property is no more available."
  in

 //Update our `props_storage` stock levels.
  let props_storage = update_item(props_kind_index,property_kind,props_storage)
  in

  let tr : transfer = {
    from_ = Tezos.self_address;
    txs = [ {
      to_ = Tezos.sender;
      property_id = abs( property_kind.id );
      amount = 1n;
    } ];
  } 
  in

  // Transfer FA2 functionality
  let entrypoint : transfer list contract = 
    match ( Tezos.get_entrypoint_opt "%transfer" property_kind.buyer_address : transfer list contract option ) with
    | None -> ( failwith "Invalid external token contract" : transfer list contract )
    | Some e -> e
  in
 
  let fa2_operation : operation =
    Tezos.transaction [tr] 0tez entrypoint
  in

  // Payout to the Publishers address.
  let receiver : unit contract =
    match (Tezos.get_contract_opt publisher_address : unit contract option) with
    | Some (contract) -> contract
    | None -> (failwith ("Not a contract") : (unit contract))
  in
 
  let payout_operation : operation = 
    Tezos.transaction unit amount receiver 
  in

 ([fa2_operation ; payout_operation], props_storage)
