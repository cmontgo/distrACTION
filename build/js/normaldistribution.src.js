
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"title":"Compute probability","name":"DistributionFunction","type":"Bool","default":false},{"title":"Compute quantile(s)","name":"QuantileFunction","type":"Bool","default":false},{"name":"QuantileFunctionType","title":"Mode for Quantiles","type":"List","options":[{"title":"central interval quantiles","name":"central"},{"title":"cumulative quantile","name":"cumulative"}],"default":"central"},{"name":"DistributionFunctionType","title":"Mode for Distribution","type":"List","options":[{"title":"P(X ≤ x1)","name":"lower"},{"title":"P(X ≥ x1)","name":"higher"},{"title":"P(x1 ≤ X ≤ x2)","name":"interval"}],"default":"lower"},{"name":"x1","title":"x1 =","type":"Number"},{"name":"p","title":"p =","type":"Number","default":0.5,"min":0,"max":1},{"name":"x2","title":"x2 =","type":"Number","default":1},{"name":"dp1","title":"Mean =","type":"Number","default":0},{"name":"dp2","title":"SD =","type":"Number","default":1}];

const view = function() {
    
    

    View.extend({
        jus: "2.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "Normal Distribution",
    jus: "2.0",
    type: "root",
    stage: 0, //0 - release, 1 - development, 2 - proposed
    controls: [
		{
			type: DefaultControls.Label,
			typeName: 'Label',
			label: "Parameters",
			controls: [
				{
					type: DefaultControls.TextBox,
					typeName: 'TextBox',
					name: "dp1",
					format: FormatDef.number
				},
				{
					type: DefaultControls.TextBox,
					typeName: 'TextBox',
					name: "dp2",
					format: FormatDef.number
				}
			]
		},
		{
			type: DefaultControls.Label,
			typeName: 'Label',
			label: "Function",
			controls: [
				{
					type: DefaultControls.LayoutBox,
					typeName: 'LayoutBox',
					style: "inline",
					controls: [
						{
							type: DefaultControls.LayoutBox,
							typeName: 'LayoutBox',
							margin: "large",
							cell: {"row":1,"column":1},
							controls: [
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "DistributionFunction"
								},
								{
									type: DefaultControls.TextBox,
									typeName: 'TextBox',
									name: "x1",
									format: FormatDef.number,
									enable: "(DistributionFunction)"
								},
								{
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									name: "DistributionFunctionType_lower",
									optionName: "DistributionFunctionType",
									optionPart: "lower",
									enable: "(DistributionFunction)"
								},
								{
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									name: "DistributionFunctionType_higher",
									optionName: "DistributionFunctionType",
									optionPart: "higher",
									enable: "(DistributionFunction)"
								},
								{
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									name: "DistributionFunctionType_interval",
									optionName: "DistributionFunctionType",
									optionPart: "interval",
									enable: "(DistributionFunction)",
									controls: [
										{
											type: DefaultControls.TextBox,
											typeName: 'TextBox',
											name: "x2",
											format: FormatDef.number,
											enable: "(DistributionFunction)"
										}
									]
								}
							]
						},
						{
							type: DefaultControls.LayoutBox,
							typeName: 'LayoutBox',
							margin: "large",
							cell: {"row":1,"column":2},
							controls: [
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "QuantileFunction"
								},
								{
									type: DefaultControls.TextBox,
									typeName: 'TextBox',
									name: "p",
									format: FormatDef.number,
									enable: "(QuantileFunction)"
								},
								{
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									name: "QuantileFunctionType_cumulative",
									optionName: "QuantileFunctionType",
									optionPart: "cumulative",
									enable: "(QuantileFunction)"
								},
								{
									type: DefaultControls.RadioButton,
									typeName: 'RadioButton',
									name: "QuantileFunctionType_central",
									optionName: "QuantileFunctionType",
									optionPart: "central",
									enable: "(QuantileFunction)"
								}
							]
						}
					]
				}
			]
		}
	]
});

module.exports = { view : view, options: options };
