import {
    Datagrid,
    List, NumberInput,
    Show,
    ShowButton,
    Create,
    SimpleForm,
    SimpleShowLayout,
    SelectInput,
    TextField, TextInput, Loading, Error
} from "react-admin";
import React from "react";
import {Link} from "react-router-dom";
import {useGetList} from "ra-core";
import {IsoOutlined} from "@material-ui/icons";

const ShowInvoiceButton = ({ record }) => {
    return (
        <div>
             {/*<ShowButton basePath="/invoice" label="Show invoice" record={record} />*/}
             <Link to={{
                 pathname: '/krondorsoft_invoice',
                 state: {
                     today: "tis vandaag he",
                     month: record.month,
                     reportEntries: record.reportEntries,
                     vatReport : record.vatReport,
                     totalDays: record.totalDays,
                     invoiceNumber : record.invoiceNumber,
                     dayOfInvoice  : record.dayOfInvoice,
                     dayOfPayment  : record.dayOfPayment
                 }
             }} >Show Krondorsoft_invoice</Link>
         </div>
     )};

export const InvoiceList = props => {
    return (
        <List {...props}>
            <Datagrid rowClick="show">
                <TextField source={"year"}/>
                <TextField source={"month"}/>
            </Datagrid>
        </List>
    );
}

export const InvoiceCreate = (props) => {
    const {
        data: monthlies,
        loading: loadingMonthlies,
        error: errorMonthlies
    } = useGetList('monthly', {
        page: 1,
        perPage: 1000
    }, {}, {});
    if (loadingMonthlies) return <Loading/>;
    if (errorMonthlies) return <Error error={"Could not load work types"}/>;
    const monthlyValues = Object.values(monthlies)
    // monthlyValues.forEach((item, n, r) => console.log(item))
    const monthlyChoices = monthlyValues.map(item => ({id: item.id, name: item.year + " " + item.month + " " + item.company.name + " " + item.customer.name}));
    return (
        <Create  {...props}>
            <SimpleForm>
                <SelectInput source={"monthlyId"} choices={monthlyChoices} />
            </SimpleForm>
        </Create>)
}


export const InvoiceShow = (props) => {
    return (
        <Show  {...props}>
            <SimpleShowLayout>
                <TextField source={"invoiceNumber"} label={"Invoice number"}/>
                <ShowInvoiceButton />
            </SimpleShowLayout>
        </Show>)
}
