import * as React from "react";
import {Route} from 'react-router-dom';
import {InvoicePaper} from './invoice_paper';
import {FixedPriceInvoice} from './fixed_price_invoice';
import {QuotePaper} from "./quote_paper";

export default [
    <Route exact path="/invoice_paper" render={(props) => <InvoicePaper {...props}
    />} noLayout/>,
    <Route exact path="/fixed_price_invoice" render={(props) => <FixedPriceInvoice {...props}
    />} noLayout/>,
    <Route exact path="/quote_paper" render={(props) => <QuotePaper {...props}
    />} noLayout/>,
];