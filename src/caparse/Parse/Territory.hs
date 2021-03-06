--------------------------------------------------------------------------------

module Parse.Territory where

import Data.Char (isLetter)

import Text.ParserCombinators.ReadP

import Parse.Common

--------------------------------------------------------------------------------

visaTerritory :: ReadP String
visaTerritory =
    leftBiasedChoice
      [ "AL" ~> "Alabama"
      , "AK" ~> "Alaska"
      , "AZ" ~> "Arizona"
      , "AR" ~> "Arkansas"
      , "CA" ~> "California"
      , "CO" ~> "Colorado"
      , "CT" ~> "Connecticut"
      , "DC" ~> "District of Columbia"
      , "DE" ~> "Delaware"
      , "FL" ~> "Florida"
      , "GA" ~> "Georgia"
      , "HI" ~> "Hawaii"
      , "ID" ~> "Idaho"
      , "IL" ~> "Illinois"
      , "IN" ~> "Indiana"
      , "IA" ~> "Iowa"
      , "KS" ~> "Kansas"
      , "KY" ~> "Kentucky"
      , "LA" ~> "Louisiana"
      , "ME" ~> "Maine"
      , "MD" ~> "Maryland"
      , "MA" ~> "Massachusetts"
      , "MI" ~> "Michigan"
      , "MN" ~> "Minnesota"
      , "MS" ~> "Mississippi"
      , "MO" ~> "Missouri"
      , "MT" ~> "Montana"
      , "NE" ~> "Nebraska"
      , "NV" ~> "Nevada"
      , "NH" ~> "New Hampshire"
      , "NJ" ~> "New Jersey"
      , "NM" ~> "New Mexico"
      , "NY" ~> "New York"
      , "NC" ~> "North Carolina"
      , "ND" ~> "North Dakota"
      , "OH" ~> "Ohio"
      , "OK" ~> "Oklahoma"
      , "OR" ~> "Oregon"
      , "PA" ~> "Pennsylvania"
      , "RI" ~> "Rhode Island"
      , "SC" ~> "South Carolina"
      , "SD" ~> "South Dakota"
      , "TN" ~> "Tennessee"
      , "TX" ~> "Texas"
      , "UT" ~> "Utah"
      , "VT" ~> "Vermont"
      , "VA" ~> "Virginia"
      , "WA" ~> "Washington"
      , "WV" ~> "West Virginia"
      , "WI" ~> "Wisconsin"
      , "WY" ~> "Wyoming"

      , "AD" ~> "Andorra"
      , "AE" ~> "United Arab Emirates"
      , "AF" ~> "Afghanistan"
      , "AG" ~> "Antigua and Barbuda"
      , "AI" ~> "Anguilla"
      , "AL" ~> "Albania"
      , "AM" ~> "Armenia"
      , "AO" ~> "Angola"
      , "AQ" ~> "Antarctica"
      , "AR" ~> "Argentina"
      , "AS" ~> "American Samoa"
      , "AT" ~> "Austria"
      , "AU" ~> "Australia"
      , "AW" ~> "Aruba"
      , "AX" ~> "Åland Islands"
      , "AZ" ~> "Azerbaijan"
      , "BA" ~> "Bosnia and Herzegovina"
      , "BB" ~> "Barbados"
      , "BD" ~> "Bangladesh"
      , "BE" ~> "Belgium"
      , "BF" ~> "Burkina Faso"
      , "BG" ~> "Bulgaria"
      , "BH" ~> "Bahrain"
      , "BI" ~> "Burundi"
      , "BJ" ~> "Benin"
      , "BL" ~> "Saint Barthélemy"
      , "BM" ~> "Bermuda"
      , "BN" ~> "Brunei" -- Brunei Darussalam
      , "BO" ~> "Bolivia" -- Bolivia, Plurinational State of
      , "BQ" ~> "Caribbean Netherlands" -- Bonaire, Sint Eustatius and Saba
      , "BR" ~> "Brazil"
      , "BS" ~> "The Bahamas" -- Bahamas
      , "BT" ~> "Bhutan"
      , "BV" ~> "Bouvet Island"
      , "BW" ~> "Botswana"
      , "BY" ~> "Belarus"
      , "BZ" ~> "Belize"
      , "CA" ~> "Canada"
      , "CC" ~> "Cocos (Keeling) Islands"
      , "CD" ~> "Democratic Republic of the Congo" -- Congo, the Democratic Republic of the
      , "CF" ~> "Central African Republic"
      , "CG" ~> "Republic of the Congo" -- Congo
      , "CH" ~> "Switzerland"
      , "CI" ~> "Côte d'Ivoire"
      , "CK" ~> "Cook Islands"
      , "CL" ~> "Chile"
      , "CM" ~> "Cameroon"
      , "CN" ~> "China"
      , "CO" ~> "Colombia"
      , "CR" ~> "Costa Rica"
      , "CU" ~> "Cuba"
      , "CV" ~> "Cabo Verde"
      , "CW" ~> "Curaçao"
      , "CX" ~> "Christmas Island"
      , "CY" ~> "Cyprus"
      , "CZ" ~> "Czech Republic"
      , "DE" ~> "Germany"
      , "DJ" ~> "Djibouti"
      , "DK" ~> "Denmark"
      , "DM" ~> "Dominica"
      , "DO" ~> "Dominican Republic"
      , "DZ" ~> "Algeria"
      , "EC" ~> "Ecuador"
      , "EE" ~> "Estonia"
      , "EG" ~> "Egypt"
      , "EH" ~> "Western Sahara"
      , "ER" ~> "Eritrea"
      , "ES" ~> "Spain"
      , "ET" ~> "Ethiopia"
      , "FI" ~> "Finland"
      , "FJ" ~> "Fiji"
      , "FK" ~> "Falkland Islands" -- Falkland Islands (Malvinas)
      , "FM" ~> "Federated States of Micronesia" -- Micronesia, Federated States of
      , "FO" ~> "Faroe Islands"
      , "FR" ~> "France"
      , "GA" ~> "Gabon"
      , "GB" ~> "United Kingdom"
      , "GD" ~> "Grenada"
      , "GE" ~> "Georgia"
      , "GF" ~> "French Guiana"
      , "GG" ~> "Guernsey"
      , "GH" ~> "Ghana"
      , "GI" ~> "Gibraltar"
      , "GL" ~> "Greenland"
      , "GM" ~> "Gambia"
      , "GN" ~> "Guinea"
      , "GP" ~> "Guadeloupe"
      , "GQ" ~> "Equatorial Guinea"
      , "GR" ~> "Greece"
      , "GS" ~> "South Georgia and the South Sandwich Islands"
      , "GT" ~> "Guatemala"
      , "GU" ~> "Guam"
      , "GW" ~> "Guinea-Bissau"
      , "GY" ~> "Guyana"
      , "HK" ~> "Hong Kong"
      , "HM" ~> "Heard Island and McDonald Islands"
      , "HN" ~> "Honduras"
      , "HR" ~> "Croatia"
      , "HT" ~> "Haiti"
      , "HU" ~> "Hungary"
      , "ID" ~> "Indonesia"
      , "IE" ~> "Ireland" -- Ireland, Republic of
      , "IL" ~> "Israel"
      , "IM" ~> "Isle of Man"
      , "IN" ~> "India"
      , "IO" ~> "British Indian Ocean Territory"
      , "IQ" ~> "Iraq"
      , "IR" ~> "Iran" -- Iran, Islamic Republic of
      , "IS" ~> "Iceland"
      , "IT" ~> "Italy"
      , "JE" ~> "Jersey"
      , "JM" ~> "Jamaica"
      , "JO" ~> "Jordan"
      , "JP" ~> "Japan"
      , "KE" ~> "Kenya"
      , "KG" ~> "Kyrgyzstan"
      , "KH" ~> "Cambodia"
      , "KI" ~> "Kiribati"
      , "KM" ~> "Comoros"
      , "KN" ~> "Saint Kitts and Nevis"
      , "KP" ~> "North Korea" -- Korea, Democratic People's Republic of
      , "KR" ~> "South Korea" -- Korea, Republic of
      , "KW" ~> "Kuwait"
      , "KY" ~> "Cayman Islands"
      , "KZ" ~> "Kazakhstan"
      , "LA" ~> "Laos" -- Lao People's Democratic Republic
      , "LB" ~> "Lebanon"
      , "LC" ~> "Saint Lucia"
      , "LI" ~> "Liechtenstein"
      , "LK" ~> "Sri Lanka"
      , "LR" ~> "Liberia"
      , "LS" ~> "Lesotho"
      , "LT" ~> "Lithuania"
      , "LU" ~> "Luxembourg"
      , "LV" ~> "Latvia"
      , "LY" ~> "Libya"
      , "MA" ~> "Morocco"
      , "MC" ~> "Monaco"
      , "MD" ~> "Moldova" -- Moldova, Republic of
      , "ME" ~> "Montenegro"
      , "MF" ~> "Collectivity of Saint Martin" -- Saint Martin (French part)
      , "MG" ~> "Madagascar"
      , "MH" ~> "Marshall Islands"
      , "MK" ~> "Republic of Macedonia" -- Macedonia, the former Yugoslav Republic of
      , "ML" ~> "Mali"
      , "MM" ~> "Myanmar"
      , "MN" ~> "Mongolia"
      , "MO" ~> "Macau" -- Macao
      , "MP" ~> "Northern Mariana Islands"
      , "MQ" ~> "Martinique"
      , "MR" ~> "Mauritania"
      , "MS" ~> "Montserrat"
      , "MT" ~> "Malta"
      , "MU" ~> "Mauritius"
      , "MV" ~> "Maldives"
      , "MW" ~> "Malawi"
      , "MX" ~> "Mexico"
      , "MY" ~> "Malaysia"
      , "MZ" ~> "Mozambique"
      , "NA" ~> "Namibia"
      , "NC" ~> "New Caledonia"
      , "NE" ~> "Niger"
      , "NF" ~> "Norfolk Island"
      , "NG" ~> "Nigeria"
      , "NI" ~> "Nicaragua"
      , "NL" ~> "Netherlands"
      , "NO" ~> "Norway"
      , "NP" ~> "Nepal"
      , "NR" ~> "Nauru"
      , "NU" ~> "Niue"
      , "NZ" ~> "New Zealand"
      , "OM" ~> "Oman"
      , "PA" ~> "Panama"
      , "PE" ~> "Peru"
      , "PF" ~> "French Polynesia"
      , "PG" ~> "Papua New Guinea"
      , "PH" ~> "Philippines"
      , "PK" ~> "Pakistan"
      , "PL" ~> "Poland"
      , "PM" ~> "Saint Pierre and Miquelon"
      , "PN" ~> "Pitcairn Islands" -- Pitcairn
      , "PR" ~> "Puerto Rico"
      , "PS" ~> "State of Palestine" -- Palestine, State of
      , "PT" ~> "Portugal"
      , "PW" ~> "Palau"
      , "PY" ~> "Paraguay"
      , "QA" ~> "Qatar"
      , "RE" ~> "Réunion"
      , "RO" ~> "Romania"
      , "RS" ~> "Serbia"
      , "RU" ~> "Russia" -- Russian Federation
      , "RW" ~> "Rwanda"
      , "SA" ~> "Saudi Arabia"
      , "SB" ~> "Solomon Islands"
      , "SC" ~> "Seychelles"
      , "SD" ~> "Sudan"
      , "SE" ~> "Sweden"
      , "SG" ~> "Singapore"
      , "SH" ~> "Saint Helena and Ascension and Tristan da Cunha" -- Saint Helena, Ascension and Tristan da Cunha
      , "SI" ~> "Slovenia"
      , "SJ" ~> "Svalbard and Jan Mayen"
      , "SK" ~> "Slovakia"
      , "SL" ~> "Sierra Leone"
      , "SM" ~> "San Marino"
      , "SN" ~> "Senegal"
      , "SO" ~> "Somalia"
      , "SR" ~> "Suriname"
      , "SS" ~> "South Sudan"
      , "ST" ~> "São Tomé and Príncipe"
      , "SV" ~> "El Salvador"
      , "SX" ~> "Sint Maarten" -- Sint Maarten (Dutch part)
      , "SY" ~> "Syria" -- Syrian Arab Republic
      , "SZ" ~> "Swaziland"
      , "TC" ~> "Turks and Caicos Islands"
      , "TD" ~> "Chad"
      , "TF" ~> "French Southern and Antarctic Lands" -- French Southern Territories
      , "TG" ~> "Togo"
      , "TH" ~> "Thailand"
      , "TJ" ~> "Tajikistan"
      , "TK" ~> "Tokelau"
      , "TL" ~> "East Timor" -- Timor-Leste
      , "TM" ~> "Turkmenistan"
      , "TN" ~> "Tunisia"
      , "TO" ~> "Tonga"
      , "TR" ~> "Turkey"
      , "TT" ~> "Trinidad and Tobago"
      , "TV" ~> "Tuvalu"
      , "TW" ~> "Taiwan" -- Taiwan, Province of China
      , "TZ" ~> "Tanzania" -- Tanzania, United Republic of
      , "UA" ~> "Ukraine"
      , "UG" ~> "Uganda"
      , "UM" ~> "United States Minor Outlying Islands"
      , "US" ~> "United States"
      , "UY" ~> "Uruguay"
      , "UZ" ~> "Uzbekistan"
      , "VA" ~> "Vatican City" -- Holy See (Vatican City State)
      , "VC" ~> "Saint Vincent and the Grenadines"
      , "VE" ~> "Venezuela" -- Venezuela, Bolivarian Republic of
      , "VG" ~> "British Virgin Islands" -- Virgin Islands, British
      , "VI" ~> "United States Virgin Islands" -- Virgin Islands, U.S.
      , "VN" ~> "Vietnam" -- Viet Nam
      , "VU" ~> "Vanuatu"
      , "WF" ~> "Wallis and Futuna"
      , "WS" ~> "Samoa"
      , "YE" ~> "Yemen"
      , "YT" ~> "Mayotte"
      , "ZA" ~> "South Africa"
      , "ZM" ~> "Zambia"
      , "ZW" ~> "Zimbabwe"

      , count 2 (satisfy isLetter)
      ]

--------------------------------------------------------------------------------
