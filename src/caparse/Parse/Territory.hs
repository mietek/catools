--------------------------------------------------------------------------------

module Parse.Territory where

import Data.Char (isLetter)

import Text.ParserCombinators.ReadP

import Parse.Common

--------------------------------------------------------------------------------

visaTerritory :: ReadP String
visaTerritory =
        stateCode
    <++ countryCode
    <++ (count 2 (satisfy isLetter))

stateCode :: ReadP String
stateCode = do
        (istring "AL" >> return "Alabama")
    <++ (istring "AK" >> return "Alaska")
    <++ (istring "AZ" >> return "Arizona")
    <++ (istring "AR" >> return "Arkansas")
    <++ (istring "CA" >> return "California")
    <++ (istring "CO" >> return "Colorado")
    <++ (istring "CT" >> return "Connecticut")
    <++ (istring "DC" >> return "District of Columbia")
    <++ (istring "DE" >> return "Delaware")
    <++ (istring "FL" >> return "Florida")
    <++ (istring "GA" >> return "Georgia")
    <++ (istring "HI" >> return "Hawaii")
    <++ (istring "ID" >> return "Idaho")
    <++ (istring "IL" >> return "Illinois")
    <++ (istring "IN" >> return "Indiana")
    <++ (istring "IA" >> return "Iowa")
    <++ (istring "KS" >> return "Kansas")
    <++ (istring "KY" >> return "Kentucky")
    <++ (istring "LA" >> return "Louisiana")
    <++ (istring "ME" >> return "Maine")
    <++ (istring "MD" >> return "Maryland")
    <++ (istring "MA" >> return "Massachusetts")
    <++ (istring "MI" >> return "Michigan")
    <++ (istring "MN" >> return "Minnesota")
    <++ (istring "MS" >> return "Mississippi")
    <++ (istring "MO" >> return "Missouri")
    <++ (istring "MT" >> return "Montana")
    <++ (istring "NE" >> return "Nebraska")
    <++ (istring "NV" >> return "Nevada")
    <++ (istring "NH" >> return "New Hampshire")
    <++ (istring "NJ" >> return "New Jersey")
    <++ (istring "NM" >> return "New Mexico")
    <++ (istring "NY" >> return "New York")
    <++ (istring "NC" >> return "North Carolina")
    <++ (istring "ND" >> return "North Dakota")
    <++ (istring "OH" >> return "Ohio")
    <++ (istring "OK" >> return "Oklahoma")
    <++ (istring "OR" >> return "Oregon")
    <++ (istring "PA" >> return "Pennsylvania")
    <++ (istring "RI" >> return "Rhode Island")
    <++ (istring "SC" >> return "South Carolina")
    <++ (istring "SD" >> return "South Dakota")
    <++ (istring "TN" >> return "Tennessee")
    <++ (istring "TX" >> return "Texas")
    <++ (istring "UT" >> return "Utah")
    <++ (istring "VT" >> return "Vermont")
    <++ (istring "VA" >> return "Virginia")
    <++ (istring "WA" >> return "Washington")
    <++ (istring "WV" >> return "West Virginia")
    <++ (istring "WI" >> return "Wisconsin")
    <++ (istring "WY" >> return "Wyoming")

countryCode :: ReadP String
countryCode = do
        (istring "AD" >> return "Andorra")
    <++ (istring "AE" >> return "United Arab Emirates")
    <++ (istring "AF" >> return "Afghanistan")
    <++ (istring "AG" >> return "Antigua and Barbuda")
    <++ (istring "AI" >> return "Anguilla")
    <++ (istring "AL" >> return "Albania")
    <++ (istring "AM" >> return "Armenia")
    <++ (istring "AO" >> return "Angola")
    <++ (istring "AQ" >> return "Antarctica")
    <++ (istring "AR" >> return "Argentina")
    <++ (istring "AS" >> return "American Samoa")
    <++ (istring "AT" >> return "Austria")
    <++ (istring "AU" >> return "Australia")
    <++ (istring "AW" >> return "Aruba")
    <++ (istring "AX" >> return "Åland Islands")
    <++ (istring "AZ" >> return "Azerbaijan")
    <++ (istring "BA" >> return "Bosnia and Herzegovina")
    <++ (istring "BB" >> return "Barbados")
    <++ (istring "BD" >> return "Bangladesh")
    <++ (istring "BE" >> return "Belgium")
    <++ (istring "BF" >> return "Burkina Faso")
    <++ (istring "BG" >> return "Bulgaria")
    <++ (istring "BH" >> return "Bahrain")
    <++ (istring "BI" >> return "Burundi")
    <++ (istring "BJ" >> return "Benin")
    <++ (istring "BL" >> return "Saint Barthélemy")
    <++ (istring "BM" >> return "Bermuda")
    <++ (istring "BN" >> return "Brunei") -- Brunei Darussalam
    <++ (istring "BO" >> return "Bolivia") -- Bolivia, Plurinational State of
    <++ (istring "BQ" >> return "Caribbean Netherlands") -- Bonaire, Sint Eustatius and Saba
    <++ (istring "BR" >> return "Brazil")
    <++ (istring "BS" >> return "The Bahamas") -- Bahamas
    <++ (istring "BT" >> return "Bhutan")
    <++ (istring "BV" >> return "Bouvet Island")
    <++ (istring "BW" >> return "Botswana")
    <++ (istring "BY" >> return "Belarus")
    <++ (istring "BZ" >> return "Belize")
    <++ (istring "CA" >> return "Canada")
    <++ (istring "CC" >> return "Cocos (Keeling) Islands")
    <++ (istring "CD" >> return "Democratic Republic of the Congo") -- Congo, the Democratic Republic of the
    <++ (istring "CF" >> return "Central African Republic")
    <++ (istring "CG" >> return "Republic of the Congo") -- Congo
    <++ (istring "CH" >> return "Switzerland")
    <++ (istring "CI" >> return "Côte d'Ivoire")
    <++ (istring "CK" >> return "Cook Islands")
    <++ (istring "CL" >> return "Chile")
    <++ (istring "CM" >> return "Cameroon")
    <++ (istring "CN" >> return "China")
    <++ (istring "CO" >> return "Colombia")
    <++ (istring "CR" >> return "Costa Rica")
    <++ (istring "CU" >> return "Cuba")
    <++ (istring "CV" >> return "Cabo Verde")
    <++ (istring "CW" >> return "Curaçao")
    <++ (istring "CX" >> return "Christmas Island")
    <++ (istring "CY" >> return "Cyprus")
    <++ (istring "CZ" >> return "Czech Republic")
    <++ (istring "DE" >> return "Germany")
    <++ (istring "DJ" >> return "Djibouti")
    <++ (istring "DK" >> return "Denmark")
    <++ (istring "DM" >> return "Dominica")
    <++ (istring "DO" >> return "Dominican Republic")
    <++ (istring "DZ" >> return "Algeria")
    <++ (istring "EC" >> return "Ecuador")
    <++ (istring "EE" >> return "Estonia")
    <++ (istring "EG" >> return "Egypt")
    <++ (istring "EH" >> return "Western Sahara")
    <++ (istring "ER" >> return "Eritrea")
    <++ (istring "ES" >> return "Spain")
    <++ (istring "ET" >> return "Ethiopia")
    <++ (istring "FI" >> return "Finland")
    <++ (istring "FJ" >> return "Fiji")
    <++ (istring "FK" >> return "Falkland Islands") -- Falkland Islands (Malvinas)
    <++ (istring "FM" >> return "Federated States of Micronesia") -- Micronesia, Federated States of
    <++ (istring "FO" >> return "Faroe Islands")
    <++ (istring "FR" >> return "France")
    <++ (istring "GA" >> return "Gabon")
    <++ (istring "GB" >> return "United Kingdom")
    <++ (istring "GD" >> return "Grenada")
    <++ (istring "GE" >> return "Georgia")
    <++ (istring "GF" >> return "French Guiana")
    <++ (istring "GG" >> return "Guernsey")
    <++ (istring "GH" >> return "Ghana")
    <++ (istring "GI" >> return "Gibraltar")
    <++ (istring "GL" >> return "Greenland")
    <++ (istring "GM" >> return "Gambia")
    <++ (istring "GN" >> return "Guinea")
    <++ (istring "GP" >> return "Guadeloupe")
    <++ (istring "GQ" >> return "Equatorial Guinea")
    <++ (istring "GR" >> return "Greece")
    <++ (istring "GS" >> return "South Georgia and the South Sandwich Islands")
    <++ (istring "GT" >> return "Guatemala")
    <++ (istring "GU" >> return "Guam")
    <++ (istring "GW" >> return "Guinea-Bissau")
    <++ (istring "GY" >> return "Guyana")
    <++ (istring "HK" >> return "Hong Kong")
    <++ (istring "HM" >> return "Heard Island and McDonald Islands")
    <++ (istring "HN" >> return "Honduras")
    <++ (istring "HR" >> return "Croatia")
    <++ (istring "HT" >> return "Haiti")
    <++ (istring "HU" >> return "Hungary")
    <++ (istring "ID" >> return "Indonesia")
    <++ (istring "IE" >> return "Ireland") -- Ireland, Republic of
    <++ (istring "IL" >> return "Israel")
    <++ (istring "IM" >> return "Isle of Man")
    <++ (istring "IN" >> return "India")
    <++ (istring "IO" >> return "British Indian Ocean Territory")
    <++ (istring "IQ" >> return "Iraq")
    <++ (istring "IR" >> return "Iran") -- Iran, Islamic Republic of
    <++ (istring "IS" >> return "Iceland")
    <++ (istring "IT" >> return "Italy")
    <++ (istring "JE" >> return "Jersey")
    <++ (istring "JM" >> return "Jamaica")
    <++ (istring "JO" >> return "Jordan")
    <++ (istring "JP" >> return "Japan")
    <++ (istring "KE" >> return "Kenya")
    <++ (istring "KG" >> return "Kyrgyzstan")
    <++ (istring "KH" >> return "Cambodia")
    <++ (istring "KI" >> return "Kiribati")
    <++ (istring "KM" >> return "Comoros")
    <++ (istring "KN" >> return "Saint Kitts and Nevis")
    <++ (istring "KP" >> return "North Korea") -- Korea, Democratic People's Republic of
    <++ (istring "KR" >> return "South Korea") -- Korea, Republic of
    <++ (istring "KW" >> return "Kuwait")
    <++ (istring "KY" >> return "Cayman Islands")
    <++ (istring "KZ" >> return "Kazakhstan")
    <++ (istring "LA" >> return "Laos") -- Lao People's Democratic Republic
    <++ (istring "LB" >> return "Lebanon")
    <++ (istring "LC" >> return "Saint Lucia")
    <++ (istring "LI" >> return "Liechtenstein")
    <++ (istring "LK" >> return "Sri Lanka")
    <++ (istring "LR" >> return "Liberia")
    <++ (istring "LS" >> return "Lesotho")
    <++ (istring "LT" >> return "Lithuania")
    <++ (istring "LU" >> return "Luxembourg")
    <++ (istring "LV" >> return "Latvia")
    <++ (istring "LY" >> return "Libya")
    <++ (istring "MA" >> return "Morocco")
    <++ (istring "MC" >> return "Monaco")
    <++ (istring "MD" >> return "Moldova") -- Moldova, Republic of
    <++ (istring "ME" >> return "Montenegro")
    <++ (istring "MF" >> return "Collectivity of Saint Martin") -- Saint Martin (French part)
    <++ (istring "MG" >> return "Madagascar")
    <++ (istring "MH" >> return "Marshall Islands")
    <++ (istring "MK" >> return "Republic of Macedonia") -- Macedonia, the former Yugoslav Republic of
    <++ (istring "ML" >> return "Mali")
    <++ (istring "MM" >> return "Myanmar")
    <++ (istring "MN" >> return "Mongolia")
    <++ (istring "MO" >> return "Macau") -- Macao
    <++ (istring "MP" >> return "Northern Mariana Islands")
    <++ (istring "MQ" >> return "Martinique")
    <++ (istring "MR" >> return "Mauritania")
    <++ (istring "MS" >> return "Montserrat")
    <++ (istring "MT" >> return "Malta")
    <++ (istring "MU" >> return "Mauritius")
    <++ (istring "MV" >> return "Maldives")
    <++ (istring "MW" >> return "Malawi")
    <++ (istring "MX" >> return "Mexico")
    <++ (istring "MY" >> return "Malaysia")
    <++ (istring "MZ" >> return "Mozambique")
    <++ (istring "NA" >> return "Namibia")
    <++ (istring "NC" >> return "New Caledonia")
    <++ (istring "NE" >> return "Niger")
    <++ (istring "NF" >> return "Norfolk Island")
    <++ (istring "NG" >> return "Nigeria")
    <++ (istring "NI" >> return "Nicaragua")
    <++ (istring "NL" >> return "Netherlands")
    <++ (istring "NO" >> return "Norway")
    <++ (istring "NP" >> return "Nepal")
    <++ (istring "NR" >> return "Nauru")
    <++ (istring "NU" >> return "Niue")
    <++ (istring "NZ" >> return "New Zealand")
    <++ (istring "OM" >> return "Oman")
    <++ (istring "PA" >> return "Panama")
    <++ (istring "PE" >> return "Peru")
    <++ (istring "PF" >> return "French Polynesia")
    <++ (istring "PG" >> return "Papua New Guinea")
    <++ (istring "PH" >> return "Philippines")
    <++ (istring "PK" >> return "Pakistan")
    <++ (istring "PL" >> return "Poland")
    <++ (istring "PM" >> return "Saint Pierre and Miquelon")
    <++ (istring "PN" >> return "Pitcairn Islands") -- Pitcairn
    <++ (istring "PR" >> return "Puerto Rico")
    <++ (istring "PS" >> return "State of Palestine") -- Palestine, State of
    <++ (istring "PT" >> return "Portugal")
    <++ (istring "PW" >> return "Palau")
    <++ (istring "PY" >> return "Paraguay")
    <++ (istring "QA" >> return "Qatar")
    <++ (istring "RE" >> return "Réunion")
    <++ (istring "RO" >> return "Romania")
    <++ (istring "RS" >> return "Serbia")
    <++ (istring "RU" >> return "Russia") -- Russian Federation
    <++ (istring "RW" >> return "Rwanda")
    <++ (istring "SA" >> return "Saudi Arabia")
    <++ (istring "SB" >> return "Solomon Islands")
    <++ (istring "SC" >> return "Seychelles")
    <++ (istring "SD" >> return "Sudan")
    <++ (istring "SE" >> return "Sweden")
    <++ (istring "SG" >> return "Singapore")
    <++ (istring "SH" >> return "Saint Helena and Ascension and Tristan da Cunha") -- Saint Helena, Ascension and Tristan da Cunha
    <++ (istring "SI" >> return "Slovenia")
    <++ (istring "SJ" >> return "Svalbard and Jan Mayen")
    <++ (istring "SK" >> return "Slovakia")
    <++ (istring "SL" >> return "Sierra Leone")
    <++ (istring "SM" >> return "San Marino")
    <++ (istring "SN" >> return "Senegal")
    <++ (istring "SO" >> return "Somalia")
    <++ (istring "SR" >> return "Suriname")
    <++ (istring "SS" >> return "South Sudan")
    <++ (istring "ST" >> return "São Tomé and Príncipe")
    <++ (istring "SV" >> return "El Salvador")
    <++ (istring "SX" >> return "Sint Maarten") -- Sint Maarten (Dutch part)
    <++ (istring "SY" >> return "Syria") -- Syrian Arab Republic
    <++ (istring "SZ" >> return "Swaziland")
    <++ (istring "TC" >> return "Turks and Caicos Islands")
    <++ (istring "TD" >> return "Chad")
    <++ (istring "TF" >> return "French Southern and Antarctic Lands") -- French Southern Territories
    <++ (istring "TG" >> return "Togo")
    <++ (istring "TH" >> return "Thailand")
    <++ (istring "TJ" >> return "Tajikistan")
    <++ (istring "TK" >> return "Tokelau")
    <++ (istring "TL" >> return "East Timor") -- Timor-Leste
    <++ (istring "TM" >> return "Turkmenistan")
    <++ (istring "TN" >> return "Tunisia")
    <++ (istring "TO" >> return "Tonga")
    <++ (istring "TR" >> return "Turkey")
    <++ (istring "TT" >> return "Trinidad and Tobago")
    <++ (istring "TV" >> return "Tuvalu")
    <++ (istring "TW" >> return "Taiwan") -- Taiwan, Province of China
    <++ (istring "TZ" >> return "Tanzania") -- Tanzania, United Republic of
    <++ (istring "UA" >> return "Ukraine")
    <++ (istring "UG" >> return "Uganda")
    <++ (istring "UM" >> return "United States Minor Outlying Islands")
    <++ (istring "US" >> return "United States")
    <++ (istring "UY" >> return "Uruguay")
    <++ (istring "UZ" >> return "Uzbekistan")
    <++ (istring "VA" >> return "Vatican City") -- Holy See (Vatican City State)
    <++ (istring "VC" >> return "Saint Vincent and the Grenadines")
    <++ (istring "VE" >> return "Venezuela") -- Venezuela, Bolivarian Republic of
    <++ (istring "VG" >> return "British Virgin Islands") -- Virgin Islands, British
    <++ (istring "VI" >> return "United States Virgin Islands") -- Virgin Islands, U.S.
    <++ (istring "VN" >> return "Vietnam") -- Viet Nam
    <++ (istring "VU" >> return "Vanuatu")
    <++ (istring "WF" >> return "Wallis and Futuna")
    <++ (istring "WS" >> return "Samoa")
    <++ (istring "YE" >> return "Yemen")
    <++ (istring "YT" >> return "Mayotte")
    <++ (istring "ZA" >> return "South Africa")
    <++ (istring "ZM" >> return "Zambia")
    <++ (istring "ZW" >> return "Zimbabwe")

--------------------------------------------------------------------------------
