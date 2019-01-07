open Events3

type evt += Event2
class obj2 = object(self)
    inherit eventHandler

    method onEvent2 : (eventHandler * evt) =
        (upcast self, Event2)

    method go =
        self#handleEvent Event2
end
