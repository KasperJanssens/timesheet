import React from 'react';
import {
    Datagrid,
    DateField,
    DateInput,
    List,
    ArrayField,
    TextField,
    TextInput,
    SelectField,
    SelectInput,
    SimpleForm,
    UrlField,
    ShowButton,
    Loading,
    Error,
    Show,
    SimpleShowLayout,
    Create,
    Edit,
    SimpleFormIterator,
    ArrayInput,
    NumberField,
    NumberInput, required
} from 'react-admin';

export const CompanyList = props => {
    return (
        <List {...props}>
            <Datagrid rowClick="show">
                <TextField source={"name"} label="Company Name"/>
                <TextField source={"vatNumber"} label={"VAT Number"}/>
                <TextField source={"addressStreet"} label={"Street"}/>
                <TextField source={"addressCity"} label={"City"}/>
            </Datagrid>
        </List>
    )
}

export const CompanyShow = props => {
    return (
        <Show {...props}>
            <SimpleShowLayout>
                <TextField source={"name"} label={"Company Name"}/>
                <TextField source={"vatNumber"} label={"VAT Number"}/>
                <TextField source={"addressStreet"} label={"Street"}/>
                <TextField source={"addressCity"} label={"City"}/>
                <TextField source={"bankAccountNumber"} label={"Bank Account Number"}/>
                <NumberField source={"lastInvoiceNumber"} label={"Last invoice number"}/>
                <NumberField source={"lastQuoteNumber"} label={"Last quote number"}/>
            </SimpleShowLayout>
        </Show>
    )
}

export const CompanyCreate = props => {
    return (
        <Create  {...props}>
            <SimpleForm>
                <TextInput source={"name"} label={"Company Name"} validate={required()}/>
                <TextInput source={"vatNumber"} label={"VAT Number"} validate={required()}/>
                <TextInput source={"addressStreet"} label={"Street"} validate={required()}/>
                <TextInput source={"addressCity"} label={"City"} validate={required()}/>
                <TextInput source={"bankAccountNumber"} label={"Bank Account Number"}
                           validate={required()}/>
                <NumberInput source={"lastInvoiceNumber"} label={"Last invoice number"}/>
                <NumberInput source={"lastQuoteNumber"} label={"Last quote number"}/>
            </SimpleForm>
        </Create>
    )
}