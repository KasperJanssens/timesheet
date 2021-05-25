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

const ShowInvoiceButton = ({record}) => {
    return (
        <div>
            <Link to={{
                pathname: '/invoice_paper',
                state: {
                    month: record.specificMonth.m,
                    reportEntries: record.monthlyReport.reportEntries,
                    vatReport: record.monthlyReport.vatReport,
                    totalDays: record.monthlyReport.totalDays,
                    invoiceNumber: record.monthlyReport.invoiceNumber,
                    dayOfInvoice: record.monthlyReport.dayOfInvoice,
                    dayOfPayment: record.monthlyReport.dayOfPayment,
                    company : record.company,
                    customer : record.customer
                }
            }} style={{color: 'blue'}} activeStyle={{color: 'red'}}>Show Invoice</Link>
        </div>
    )
};

export const InvoiceList = props => {
    return (
        <List {...props}>
            <Datagrid rowClick="show">
                <TextField source={"specificMonth.y"} label={"year"}/>
                <TextField source={"specificMonth.m"} label={"month"}/>
                <TextField source={"customer.name"} label={"customer"}/>
                <TextField source={"company.name"} label={"company"}/>
                <TextField source={"monthlyReport.invoiceNumber"} label={"invoice number"}/>
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
    const monthlyChoices = monthlyValues.map(item => ({
        id: item.id,
        name: item.year + " " + item.month + " " + item.company.name + " " + item.customer.name
    }));
    return (
        <Create  {...props}>
            <SimpleForm>
                <SelectInput source={"monthlyId"} choices={monthlyChoices}/>
            </SimpleForm>
        </Create>)
}


export const InvoiceShow = (props) => {
    return (
        <Show  {...props}>
            <SimpleShowLayout>
                <TextField source={"specificMonth.y"} label={"year"}/>
                <TextField source={"specificMonth.m"} label={"month"}/>
                <TextField source={"customer.name"} label={"customer"}/>
                <TextField source={"company.name"} label={"company"}/>
                <TextField source={"monthlyReport.invoiceNumber"} label={"invoice number"}/>
                <TextField source={"monthlyReport.vatReport.totalExcl"} label={"total excl vat"}/>
                <TextField source={"monthlyReport.vatReport.total"} label={"total"}/>
                <ShowInvoiceButton/>
            </SimpleShowLayout>
        </Show>)
}
