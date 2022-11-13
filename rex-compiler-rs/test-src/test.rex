<test name={ <div></div> }>
    {
        for product in products {
            for product1 in products {
                product1.name
            }
        }
    }
</test>