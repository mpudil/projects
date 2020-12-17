import dash
import dash_core_components as dcc
import dash_html_components as html
import pandas as pd
import cv2
from gaze_tracking import GazeTracking
from tracker import run_gazetracker as rg
import time

external_stylesheets = ['https://codepen.io/chriddyp/pen/bWLwgP.css']

app = dash.Dash(__name__, external_stylesheets=external_stylesheets)

server = app.server

app.layout = html.Div([
    html.H2('GazeTracker'),
    dcc.Dropdown(
        id='dropdown',
        options=[{'label': '1 minute', 'value': 60}],
        value=1000
    ),
    html.Div(id='display-value')
])

@app.callback(dash.dependencies.Output('display-value', 'children'),
              [dash.dependencies.Input('dropdown', 'value')])


def display_value(value):
    rg(value)

if __name__ == '__main__':
    app.run_server(debug=True)