import {
    Datagrid,
    List, NumberInput,
    Show,
    ShowButton,
    Create,
    SimpleForm,
    SimpleShowLayout,
    SelectInput,
    TextField, TextInput, Loading, Error, SelectField
} from "react-admin";
import React from "react";
import {Link} from "react-router-dom";
import {useGetList} from "ra-core";
import {toSelectCompanies, toSelectCustomers} from "./helper";


export const QuoteList = props => {
    const {
        data: companies,
        loading: loadingCompanies,
        error: errorCompanies
    } = useGetList('company', {
        page: 1,
        perPage: 1000
    }, {}, {});

    const {
        data: customers,
        loading: loadingCustomers,
        error: errorCustomers
    } = useGetList('customer', {
        page: 1,
        perPage: 1000
    }, {}, {});

    if (loadingCompanies) return <Loading/>;
    if (errorCompanies) return <Error error={"Could not load companies"}/>;
    if (loadingCustomers) return <Loading/>;
    if (errorCustomers) return <Error error={"Could not load customers"}/>;

    let selectCustomers = toSelectCustomers(Object.values(customers))
    let selectCompanies = toSelectCompanies(Object.values(companies))
    return (
        <List {...props}>
            <Datagrid rowClick="show">
                <TextField source={"quoteId"} label={"Quote number"}/>
                <SelectField source={"customer.id"} choices={selectCustomers}
                             label={"customer"}/>
                <SelectField source={"company.vatNumber"} choices={selectCompanies}
                             label={"company"}/>
            </Datagrid>
        </List>
    );
}

export const QuoteCreate = (props) => {
    const {
        data: companies,
        loading: loadingCompanies,
        error: errorCompanies
    } = useGetList('company', {
        page: 1,
        perPage: 1000
    }, {}, {});

    const {
        data: customers,
        loading: loadingCustomers,
        error: errorCustomers
    } = useGetList('customer', {
        page: 1,
        perPage: 1000
    }, {}, {});

    if (loadingCustomers) return <Loading/>;
    if (errorCustomers) return <Error error={"Could not load work types"}/>;
    if (loadingCompanies) return <Loading/>;
    if (errorCompanies) return <Error error={"Could not load companies"}/>;
    let selectCustomers = toSelectCustomers(Object.values(customers))
    let selectCompanies = toSelectCompanies(Object.values(companies))
    return (
        <Create  {...props}>
            <SimpleForm>
                <SelectInput source={"companyId"} choices={selectCompanies} />
                <SelectInput source={"customerId"} choices={selectCustomers} />
                <NumberInput source="total"  label={"Fixed price total (excluding vat, in euros)"}/>
            </SimpleForm>
        </Create>)
}


export const QuoteShow = (props) => {
    const {
        data: companies,
        loading: loadingCompanies,
        error: errorCompanies
    } = useGetList('company', {
        page: 1,
        perPage: 1000
    }, {}, {});

    const {
        data: customers,
        loading: loadingCustomers,
        error: errorCustomers
    } = useGetList('customer', {
        page: 1,
        perPage: 1000
    }, {}, {});

    if (loadingCompanies) return <Loading/>;
    if (errorCompanies) return <Error error={"Could not load companies"}/>;
    if (loadingCustomers) return <Loading/>;
    if (errorCustomers) return <Error error={"Could not load customers"}/>;

    let selectCustomers = toSelectCustomers(Object.values(customers))
    let selectCompanies = toSelectCompanies(Object.values(companies))
    return (
        <Show  {...props}>
            <SimpleShowLayout>
                <TextField source={"quoteId"} label={"Quote number"}/>
                <TextField source={"vatReport.totalExcl"} label={"Total excluding VAT"}/>
                <TextField source={"vatReport.totalVAT"} label={"VAT amount"}/>
                <TextField source={"vatReport.total"} label={"total with VAT"}/>
                <SelectField source={"customer.id"} choices={selectCustomers}
                             label={"choose customer"}/>
                <SelectField source={"company.vatNumber"} choices={selectCompanies}
                             label={"choose company"}/>
            </SimpleShowLayout>
        </Show>)
}
