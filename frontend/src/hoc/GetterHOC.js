import React, { Component } from 'react'
import request from 'request'
import Redbox from 'redbox-react'

export default (ChildComponent, getter) => (
  class GetterComponent extends Component {
    constructor(props) {
      super(props)
      this.state = {
        childProps: {},
        gettingFailed: false
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

              this.setState({childProps: newChildProps})

            } else {
              this.setState({gettingFailed: true})
            }
          })
      })
    }

    render() {
      const { childProps, gettingFailed } = this.state

      if (gettingFailed) {

        return <RedBox error={"Failed to fetch data from the backend"} />

      } else {

        const allChildProps = Object.assign({},
          this.props,
          childProps
        )
        return <ChildComponent {...allChildProps} />
      }
    }
  }
)
