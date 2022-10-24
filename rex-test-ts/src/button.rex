<test name={ 5 + 4 }>
    {
        for product in products {
            <div click={ onclick() } id={ product.id }>{ product.name }</div>
        }
    }
</test>