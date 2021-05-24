import * as React from "react";
import {Route} from 'react-router-dom';
import {KrondorsoftInvoice} from './krondorsoft_invoice';
import {FixedPriceInvoice} from './fixed_price_invoice';
import {QuotePaper} from "./quote_paper";

export default [
    <Route exact path="/krondorsoft_invoice" render={(props) => <KrondorsoftInvoice {...props}
    />} noLayout/>,
    <Route exact path="/fixed_price_invoice" render={(props) => <FixedPriceInvoice {...props}
    />} noLayout/>,
    <Route exact path="/quote_paper" render={(props) => <QuotePaper {...props}
    />} noLayout/>,
];