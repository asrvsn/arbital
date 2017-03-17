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

    const requireAuth = (nextState, replace) => {
      replace('/login')
    }

    const children = (
      <div>
        <Header />
        <Route path='/'>
          <IndexRoute component={Feed} onEnter={requireAuth} />

          <Route path='login' component={Login} />
          <Route path='logout' component={Logout} onEnter={requireAuth} />

          <Route path='arguments'>
            <Route path=':argumentid' component={Argument} onEnter={requireAuth} />
          </Route>

          <Route path='claims'>
            <Route path=':claimid'>
              <IndexRoute component={Claim} onEnter={requireAuth} />
              <Route path='for' component={CreateArgument} onEnter={requireAuth} />
              <Route path='against' component={CreateArgument} onEnter={requireAuth} />
            </Route>
            <Route path='create' component={CreateClaim} onEnter={requireAuth} />
          </Route>

          <Route path='users'>
            <Route path=':userid' component={User} onEnter={requireAuth} />
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
