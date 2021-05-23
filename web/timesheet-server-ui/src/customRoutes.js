import * as React from "react";
import {Route} from 'react-router-dom';
import {KrondorsoftInvoice} from './krondorsoft_invoice';
import {KrondorsoftFixedPriceInvoice} from './krondorsoft_fixed_price_invoice';

export default [
    <Route exact path="/krondorsoft_invoice" render={(props) => <KrondorsoftInvoice {...props}
    />} noLayout/>,
    <Route exact path="/krondorsoft_fixed_price_invoice" render={(props) => <KrondorsoftFixedPriceInvoice {...props}
    />} noLayout/>,
];