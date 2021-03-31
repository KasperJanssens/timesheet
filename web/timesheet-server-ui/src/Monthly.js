import {Datagrid, List, Show, ShowButton, SimpleShowLayout, TextField} from "react-admin";
import React from "react";
import {Link} from "react-router-dom";

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

export const MonthlyList = props => {
    return (
        <List {...props}>
            <Datagrid rowClick="show">
                <TextField source={"year"}/>
                <TextField source={"month"}/>
            </Datagrid>
        </List>
    );
}

export const MonthlyShow = (props) => {
    return (
        <Show title="Krondorsoft_invoice" {...props}>
            <SimpleShowLayout>
                <TextField source={"invoiceNumber"} label={"Invoice number"}/>
                <ShowInvoiceButton />
            </SimpleShowLayout>
        </Show>)
}

