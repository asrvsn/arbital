import React, { Component, PropTypes } from 'react'
import { browserHistory, Router, Route, IndexRoute } from 'react-router'
import { Provider } from 'react-redux'

import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';

import Header from '../components/Header'
import Feed from '../components/Feed'
import Argument from '../components/Argument'
import Claim from '../components/Claim'
import CreateArgument from '../components/CreateArgument'
import CreateClaim from '../components/CreateClaim'
import Login from '../components/Login'
import Logout from '../components/Logout'
import User from '../components/User'


class AppContainer extends Component {
  static propTypes = {
    store  : PropTypes.object.isRequired
  }

  shouldComponentUpdate () {
    return false
  }

  render () {
    const { store } = this.props
    const children = (
      <div>
        <Header />
        <Route path='/' component={Feed}>

          <Route path='arguments'>
            <Route path=':argumentid' component={Argument} />
          </Route>

          <Route path='claims'>
            <Route path=':claimid'>
              <IndexRoute component={Claim} />
              <Route path='for' component={CreateArgument} />
              <Route path='against' component={CreateArgument} />
            </Route>
            <Route path='create' component={CreateClaim} />
          </Route>

          <Route path='users'>
            <Route path='login' component={Login} />
            <Route path='logout' component={Logout} />
            <Route path=':userid' component={User} />
          </Route>
        </Route>
      </div>
    )

    return (
      <Provider store={store}>
        <MuiThemeProvider>
          <div style={{ height: '100%' }}>
            <Router history={browserHistory} children={children} />
          </div>
        </MuiThemeProvider>
      </Provider>
    )
  }
}


export default AppContainer
