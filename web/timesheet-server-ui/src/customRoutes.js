import * as React from "react";
import { Route } from 'react-router-dom';
import {KrondorsoftInvoice} from './krondorsoft_invoice';

export default [
    <Route exact path="/krondorsoft_invoice" render={(props) => <KrondorsoftInvoice {...props}
    /> } noLayout />,
];