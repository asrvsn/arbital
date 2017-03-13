import React, { Component } from 'react'
import request from 'request'
import Redbox from 'redbox-react'

import LinearProgress from 'material-ui/LinearProgress';

export default (ChildComponent, getter) => (
  class GetterComponent extends Component {
    constructor(props) {
      super(props)
      this.state = {
        childProps: {},
        gettingState: 'LOADING'
      }
      this.backendUrl = window.location.hostname + '5000'
    }

    componentDidMount() {
      getter(this.props).forEach(item => {
        const {path, mapResponseToProps} = item
        request
          .get(this.backendUrl + path)
          .on('response', response => {

            if (response.statusCode == 200) {

              const { childProps } = this.state
              const newChildProps = Object.assign(
                childProps,
                mapResponseToProps(response)
              )

              this.setState({childProps: newChildProps, gettingState: 'SUCCEEDED'})

            } else {
              this.setState({gettingState: 'FAILED'})
              console.error(response)
            }
          })
      })
    }

    render() {
      const { childProps, gettingState } = this.state

      switch(gettingState) {
        case 'LOADING':
          return <LinearProgress mode="indeterminate" />
        case 'FAILED':
          return <RedBox error={"Failed to fetch data from the backend"} />
        case 'SUCCEEDED': {
          const allChildProps = Object.assign({},
            this.props,
            childProps
          )
          return <ChildComponent {...allChildProps} />
        }

      }
    }
  }
)
