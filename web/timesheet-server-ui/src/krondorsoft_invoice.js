import React from 'react';

export class KrondorsoftInvoice extends React.Component {

    constructor(props) {
        super(props)
        console.log(props.location.state)
        this.tableElems = []
        for (let i = 0; i < props.location.state.reportEntries.length; i++) {
            let reportEntry = props.location.state.reportEntries[i]
            console.log(reportEntry)
            this.tableElems.push(<tr>
                <td>{reportEntry.omschrijving}</td>
                <td id="aantalUren" className="amount">{reportEntry.aantalUur}</td>
                <td id="tarief" className="amount">{reportEntry.tarief}</td>
                <td id="totaalZonderBtw" className="amount">{reportEntry.subTotaal}</td>
            </tr>)
        }
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
                        <p><strong>Unifly</strong></p>
                        <p>Luchthavenlei 7a</p>
                        <p>2100 Antwerpen</p>
                        <p>BTW BE 0635 520 937</p>
                    </section>
                    <section>
                        <h2>Diensten geleverd door:</h2>
                        <p><strong>Krondorsoft B.V.B.A.</strong></p>
                        <p>Ottergemsesteenweg 354</p>
                        <p>9000 Gent</p>
                        <p>BTW BE 0893 815 606</p>
                    </section>
                    <section>
                        <h2>Maand waarop betrekking:</h2>
                        <p>{this.props.location.state.month}</p>
                    </section>
                </div>
                <section class="payment-info">
                    <p>Gelieve te betalen voor <strong id="vervalDatum">{this.props.location.state.dayOfPayment}</strong> op
                        het rekeningnummer <strong>BE23
                            6451 2777
                            4091</strong> (BIC: JVBA BE 22 )</p>
                </section>
            </header>
            <main>
                <section class="calculation">
                    <section class="detail-view">
                        <table>
                            <thead>
                            <th>omschrijving</th>
                            <th class="amount">aantal(h)</th>
                            <th class="amount">tarief</th>
                            <th class="amount">subtotaal</th>
                            </thead>
                            <tbody>
                            {this.tableElems}
                            {/*<tr>*/}
                            {/*    <td>MEET divers</td>*/}
                            {/*    <td id="aantalUren" class="amount">15</td>*/}
                            {/*    <td id="tarief" class="amount">75,00</td>*/}
                            {/*    <td id="totaalZonderBtw" class="amount">1125,00</td>*/}
                            {/*</tr>*/}
                            {/*<tr>*/}
                            {/*    <td>IMPL Implementeren basisversie vluchtvalidatie</td>*/}
                            {/*    <td id="aantalUren" class="amount">169</td>*/}
                            {/*    <td id="tarief" class="amount">75,00</td>*/}
                            {/*    <td id="totaalZonderBtw" class="amount">12675,00</td>*/}
                            {/*</tr>*/}
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