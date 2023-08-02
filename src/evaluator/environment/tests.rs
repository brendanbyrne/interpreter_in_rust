use super::*;

#[test]
fn env() {
    let root = Rc::new(RefCell::new(Env::default()));
    let child = Env::new_with_parent(Rc::clone(&root));

    let id = String::from("x");
    let value = Object::Int(1);

    root.borrow_mut().set(id.clone(), value.clone());

    assert_eq!(child.get(&id).unwrap(), value);
}
