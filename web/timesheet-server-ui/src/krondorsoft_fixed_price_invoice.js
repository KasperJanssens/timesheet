import React from 'react';

export class KrondorsoftFixedPriceInvoice extends React.Component {

    constructor(props) {
        super(props)
        console.log(props.location.state)
    }

    render() {
        return <div class="page invoice">
            <header>
                <img class="logo" src="krondorsoft.png" type="image/svg+xml"/>
                <div class="invoice-data">
                    <span id="vandaag">{this.props.location.state.dayOfInvoice}</span>
                    <h1>Factuur</h1>
                    <span class="number">{this.props.location.state.invoiceNumber}</span>
                </div>
                <div class="address">
                    <section>
                        <h2>Diensten geleverd aan:</h2>
                        <p><strong>{this.props.location.state.customer.name}</strong></p>
                        <p>{this.props.location.state.customer.addressStreet}</p>
                        <p>{this.props.location.state.customer.addressCity}</p>
                        <p>{this.props.location.state.customer.vatNumber}</p>
                    </section>
                    <section>
                        <h2>Diensten geleverd door:</h2>
                        <p><strong>{this.props.location.state.company.name}</strong></p>
                        <p>{this.props.location.state.company.addressStreet}</p>
                        <p>{this.props.location.state.company.addressCity}</p>
                        <p>{this.props.location.state.company.vatNumber}</p>
                    </section>
                </div>
                <section class="payment-info">
                    <p>Gelieve te betalen voor <strong id="vervalDatum">{this.props.location.state.dayOfPayment}</strong> op
                        het rekeningnummer <strong>{this.props.location.state.company.vatNumber}</strong> (BIC: JVBA BE 22 )</p>
                </section>
            </header>
            <main>
                <section class="calculation">
                    <section class="detail-view">
                        <table>
                            <thead>
                            <th>omschrijving</th>
                            <th class="amount">subtotaal</th>
                            </thead>
                            <tbody>
                            <tr>
                                 <td>First month of care package</td>
                                 <td id="totaalZonderBtw" className="amount">{this.props.location.state.vatReport.totalExcl}</td>

                            </tr>
                            </tbody>
                        </table>
                    </section>

                </section>
                <section class="total">
                    <table>
                        <tr>
                            <td>totaal aantal dagen</td>
                            <td id="totaalAantalDagen" class="amount">{this.props.location.state.totalDays}</td>
                        </tr>
                        <tr>
                            <td>totaal excl BTW</td>
                            <td id="totaalExclusiefBtw" class="amount">{this.props.location.state.vatReport.totalExcl}</td>
                        </tr>
                        <tr>
                            <td>BTW tarief</td>
                            <td class="amount">21%</td>
                        </tr>
                        <tr>
                            <td>BTW bedrag</td>
                            <td id="btwBedrag" class="amount">{this.props.location.state.vatReport.totalVAT}</td>
                        </tr>
                        <tr>
                            <td><strong>totaal</strong></td>
                            <td id="totaalMetBtw" class="amount"><strong>{this.props.location.state.vatReport.total}</strong></td>
                        </tr>
                    </table>
                </section>

            </main>
        </div>
    }
}