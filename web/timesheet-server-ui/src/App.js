import logo from './logo.svg';
import './App.css';

import jsonServerProvider from "ra-data-json-server";
import {Admin, Resource, ListGuesser} from 'react-admin';
import {DailyCreate, DailyEdit, DailyList, DailyShow} from "./Daily";
import customRoutes from "./customRoutes";
import MyLayout from "./MyLayout";
import {MonthlyList, MonthlyShow} from "./Monthly";
import {

    CustomerCreate,
    CustomerList,
    CustomerShow
} from "./Customer";
import {InvoiceCreate, InvoiceList, InvoiceShow} from "./invoices";
import {CompanyCreate, CompanyList, CompanyShow} from "./Company";

const React = require('react');
const ReactDOM = require('react-dom');

const josDeUrl = process.env.REACT_APP_ADMIN_SERVER;
const serviceProvider = jsonServerProvider(josDeUrl + '/v1')

const App = () => (
    <Admin
        customRoutes={customRoutes}
        dataProvider={serviceProvider}>
        <Resource id="daily" name="daily" list={DailyList} create={DailyCreate} edit={DailyEdit}/>
        <Resource id="monthly" name="monthly" list={MonthlyList} show={MonthlyShow}/>
        <Resource id={"invoice"} name={"invoice"} list={InvoiceList} create={InvoiceCreate}
                  show={InvoiceShow}/>
        <Resource id={"customer"} name={"customer"} list={CustomerList} show={CustomerShow}
                  create={CustomerCreate}/>
        <Resource id={"company"} name={"company"} list={CompanyList} show={CompanyShow}
                  create={CompanyCreate}/>
        <Resource id="worktype" name="worktype" list={ListGuesser}/>

    </Admin>
);

export default App;
