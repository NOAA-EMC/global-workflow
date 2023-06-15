from pygw.factory import Factory


class Class1:
    def __init__(self):
        self.my_name = 'Class1'


class Class2:
    def __init__(self):
        self.my_name = 'Class2'


def test_factory():
    _ = Factory('Test0')
    try:
        from pygw.factory import Test0Factory
    # linter will likely throw an error here since 'Test0Factory' is not a valid module until runtime
    except ModuleNotFoundError:
        raise AssertionError("'Test0Factory' was not found in the 'pygw.factory' module")


def test_register():
    factory = Factory('Test1')
    try:
        factory.register('Class1', Class1)
    except Exception:
        raise AssertionError("Unable to register 'Class1' in 'Test1Factory'")


def test_create():
    factory = Factory('Test2')
    factory.register('Class1', Class1)
    factory.register('Class2', Class2)

    c1 = factory.create('Class1')
    assert c1.my_name == 'Class1', "Error in creating builder 'Class1' in Factory 'Test2Factory'"
    c2 = factory.create('Class2')
    assert c2.my_name == 'Class2', "Error in creating builder 'Class2' in Factory 'Test2Factory'"


def test_destroy():
    factory = Factory('Test3')
    factory.register('Class1', Class1)
    factory.register('Class2', Class2)

    _ = factory.create('Class1')
    _ = factory.create('Class2')
    factory.destroy('Class2')
    assert not (factory.is_registered('Class2'))


def test_registered():
    factory = Factory('Test4')
    factory.register('Class1', Class1)
    factory.register('Class2', Class2)

    _ = factory.create('Class1')
    _ = factory.create('Class2')
    assert factory.registered == {'Class1', 'Class2'}, "Unregistered builders in 'Test4Factory'"


def test_is_registered():
    factory = Factory('Test5')
    factory.register('Class1', Class1)

    _ = factory.create('Class1')
    assert factory.is_registered('Class1'), "Unregistered builder in 'Test5Factory'"
    assert not (factory.is_registered('Class3')), "Unregistered builder in 'Test5Factory'"


def test_get_factory():
    factory = Factory('Test6')
    assert Factory.get_factory('Test6Factory') == factory
