import {
    Datagrid,
    List, NumberInput,
    Show,
    ShowButton,
    BooleanInput,
    Create,
    SimpleForm,
    SimpleShowLayout,
    SelectInput,
    TextField, TextInput, Loading, Error, SelectField
} from "react-admin";
import React from "react";
import {Link} from "react-router-dom";
import {useGetList} from "ra-core";
import {toSelectCompanies, toSelectCustomers, toSelectQuotes} from "./helper";
import {useFormState} from "react-final-form";


export const FixedPriceInvoiceList = props => {
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
                <TextField source={"invoiceId"} label={"fixed price invoice number"}/>
                <SelectField source={"customer.id"} choices={selectCustomers}
                             label={"choose customer"}/>
                <SelectField source={"company.vatNumber"} choices={selectCompanies}
                             label={"choose company"}/>
                <TextField source={"dayOfInvoice"} label={"Day of invoice"}/>
                <TextField source={"dayOfInvoice"} label={"Day of invoice"}/>
                <TextField source={"vatReport.totalExcl"} label={"Total excl vat"}/>
                <TextField source={"vatReport.total"} label={"Total"}/>
            </Datagrid>
        </List>
    );
}

const ShowInvoiceButton = ({ record }) => {
    return (

            <Link to={{
                pathname: '/fixed_price_invoice',
                state: {
                    today: "tis vandaag he",
                    month: record.month,
                    customer: record.customer,
                    company: record.company,
                    vatReport : record.vatReport,
                    totalDays: record.totalDays,
                    invoiceNumber : record.invoiceNumber,
                    dayOfInvoice  : record.dayOfInvoice,
                    dayOfPayment  : record.dayOfPayment,
                    description : record.description
                }
            }} style={{color: 'blue'}} activeStyle={{color: 'red'}} >Show Invoice</Link>

    )};

const ChooseQuoteOrFixedPrice = (props) => {
    const {values} = useFormState();
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
    const {
        data: quotes,
        loading: loadingQuotes,
        error: errorQuotes
    } = useGetList('quote', {
        page: 1,
        perPage: 1000
    }, {}, {filter: 'uninvoiced'});

    if (loadingCompanies) return <Loading/>;
    if (errorCompanies) return <Error error={"Could not load companies"}/>;
    if (loadingCustomers) return <Loading/>;
    if (errorCustomers) return <Error error={"Could not load customers"}/>;
    if (loadingQuotes) return <Loading/>;
    if (errorQuotes) return <Error error={"Could not load quotes"}/>;

    let selectCustomers = toSelectCustomers(Object.values(customers))
    let selectCompanies = toSelectCompanies(Object.values(companies))
    let selectQuotes = toSelectQuotes(Object.values(quotes))


    return (<div class="ra-input"><BooleanInput label="From Quote" source="fromQuote"/>
            {values.fromQuote ?
                <div class="ra-input"><SelectInput source="invoice.Right" choices={selectQuotes} label={"Choose quote"}/></div> :
                <div>
                    <div class="ra-input"><NumberInput source="invoice.Left.total" label={"Total in euros excl vat"}/></div>
                    <div class="ra-input"><SelectInput source={"invoice.Left.customerId"} choices={selectCustomers}
                                 label={"choose customer"}/></div>
                    <div class="ra-input"><SelectInput source={"invoice.Left.companyId"} choices={selectCompanies}
                                 label={"choose company"}/></div>
                     <div class="ra-input"><TextInput source={"invoice.Left.description"} label={"Description"}/></div>
                </div>
            }
        </div>
    )

}

export const FixedPriceInvoiceCreate = (props) => {
    return (
        <Create  {...props}>
            <SimpleForm>
                <ChooseQuoteOrFixedPrice/>
            </SimpleForm>
        </Create>)
}


export const FixedPriceInvoiceShow = (props) => {
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
                <TextField source={"invoiceId"} label={"fixed price invoice number"}/>
                <SelectField source={"customer.id"} choices={selectCustomers}
                             label={"choose customer"}/>
                <SelectField source={"company.vatNumber"} choices={selectCompanies}
                             label={"choose company"}/>
                <TextField source={"dayOfInvoice"} label={"Day of invoice"}/>
                <TextField source={"dayOfPayment"} label={"Day of payment"}/>
                <ShowInvoiceButton />
            </SimpleShowLayout>
        </Show>)
}
